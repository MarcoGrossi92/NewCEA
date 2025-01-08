module CEA_module
  implicit none
  private
  !public:: CEAcopywrite

  type, public:: obj_propellant
    real(8):: T, wt
    character(len=15):: name
    character(len=1):: type
    character(len=100):: more
  end type obj_propellant

  type, public:: obj_CEA_species
    integer:: n
    real(8), dimension(:), allocatable:: massf, molef
    character(len=20), dimension(:), allocatable:: name
  end type obj_CEA_species

  type, public:: obj_CEA_out
    real(8):: temperature
    real(8):: pressure
    real(8):: w
    real(8):: cp, cpeq
    real(8):: gamma
    real(8):: a
    real(8):: Mach
    real(8):: h0
    real(8):: h
    real(8):: ivac
    real(8):: cstar
    real(8):: cf
    type(obj_CEA_species):: species
  end type obj_CEA_out

  type, public:: obj_CEA
    character(len=4), public:: input_mode='data'
    integer:: nprop
    integer:: indx
    type(obj_propellant), dimension(:), allocatable:: prop
    character(len=3):: eps_type='sup'
    real(8):: pressure
    real(8):: OF
    real(8):: epsilon
    type(obj_CEA_out):: FE
    type(obj_CEA_out):: SE
    logical:: only=.false.
    logical:: OG=.false.
  contains
    procedure, public :: solve
    procedure         :: recompute_fractions
  end type

  contains

  function libpath() result(result)
    implicit none
    character(len=500) :: result
    character(len=500) :: masterpath

    ! Assign correct path depending on ENV variable
    call get_environment_variable('NewCEADIR',masterpath)
    result = trim(masterpath)//'/lib/thermo-transport/'

  end function libpath

  subroutine solve(self,filename)
    use CEAinc, only: MAXNGC
    implicit none
    class(obj_CEA), intent(inout) :: self
    character(*), intent(inout) :: filename
    external :: cea2
    real(8) :: state(2,6), perfo(2,3)
    real(8) :: spec_frac(MAXNGC)
    character(20) :: spec_name(MAXNGC)
    integer :: i

    call cea2 (libpath(), filename, self%indx, spec_name, spec_frac, perfo, state)

    ! Performance
    self%SE%cstar = perfo(1,1); self%SE%cf = perfo(1,2); self%SE%ivac = perfo(1,3)
    self%FE%cstar = perfo(2,1); self%FE%cf = perfo(2,2); self%FE%ivac = perfo(2,3)

    ! Flow state
    self%SE%pressure = state(1,1); self%SE%temperature = state(1,2); self%SE%h = state(1,3)
    self%FE%pressure = state(2,1); self%FE%temperature = state(2,2); self%FE%h = state(2,3)
    self%SE%w = state(1,4); self%SE%a = state(1,5); self%SE%Mach = state(1,6)
    self%FE%w = state(2,4); self%FE%a = state(2,5); self%FE%Mach = state(2,6)

    self%SE%h0 = self%SE%h + 0.5d0*(self%SE%Mach*self%SE%a)**2.d0
    self%FE%h0 = self%FE%h + 0.5d0*(self%FE%Mach*self%FE%a)**2.d0

    ! Composition
    self%SE%species%n = 0
    do i = 1, MAXNGC
      if (spec_name(i)=='GAME_OVER') exit
      self%SE%species%n = self%SE%species%n + 1
    end do
    allocate(self%SE%species%name(1:self%SE%species%n))
    allocate(self%SE%species%massf(1:self%SE%species%n))
    self%SE%species%name = spec_name(1:self%SE%species%n)
    self%SE%species%massf = spec_frac(1:self%SE%species%n)

    if (self%OG) call self%recompute_fractions()

  end subroutine solve

  pure subroutine recompute_fractions(self)
    implicit none
    class(obj_CEA), intent(inout) :: self
    integer :: nsc, ncond, s
    character(20), allocatable :: dummy_name(:)
    real(8), allocatable :: dummy_fraction(:)
    real(8) :: yCondP

    nsc = self%SE%species%n

    allocate(dummy_name(1:nsc))
    allocate(dummy_fraction(1:nsc))
    dummy_name = self%SE%species%name
    if (allocated(self%SE%species%massf)) then
      dummy_fraction = self%SE%species%massf
    else
      dummy_fraction = self%SE%species%molef
    endif
    ncond = 0; yCondP = 0.d0

    do s = 1, nsc
      if (index(dummy_name(s),'(L)')/=0 .or. index(dummy_name(s),'(cr)')/=0) then
        ncond = ncond+1
        !pos = INDEX(full_string, '(')
        !good_name = dummy_name(1:MAX(1, pos-1))
        yCondP = yCondP+dummy_fraction(s)
      endif
    end do

    if (ncond > 0) then 
      self%SE%species%n = nsc-ncond

      deallocate(self%SE%species%name)
      allocate(self%SE%species%name(1:nsc-ncond))
      if (allocated(self%SE%species%massf)) then
        deallocate(self%SE%species%massf)
        allocate(self%SE%species%massf(1:nsc-ncond))
      else
        deallocate(self%SE%species%molef)
        allocate(self%SE%species%molef(1:nsc-ncond))
      endif

      do s = 1, nsc
        if (index(dummy_name(s),'(L)')/=0 .or. index(dummy_name(s),'(cr)')/=0) cycle
        self%SE%species%name(s) = dummy_name(s)
        if (allocated(self%SE%species%massf)) then
          self%SE%species%massf(s) = dummy_fraction(s)/(1-yCondP)
        else
          self%SE%species%molef(s) = dummy_fraction(s)/(1-yCondP)
        endif        
      end do
    endif

  end subroutine recompute_fractions

!   subroutine CEAcopywrite(rows_number,values_to_replace,eps_row,eps_type)
!     implicit none
!     integer, dimension(:), intent(in):: rows_number
!     integer, intent(in):: eps_row
!     real, dimension(:), intent(in):: values_to_replace
!     character(len=*), intent(in):: eps_type
!     character(len=200):: stringa, args(2)
!     integer:: j, i

!     open(newunit=unitFile,file='CEAfile.inp')
!     rewind(unit_INP)
    
!     !> Copy all j-lines before row_value(1); write parameter value in row_value(1)
!     if (rows_number(1)>0) then
!       do j = 1, rows_number(1)-1
!         read(unit_INP,'(A)') stringa
!         write(unitFile,'(A)') stringa
!       enddo
!       read(unit_INP,'(A)') stringa
!       call parse(stringa,'=',args)
!       stringa = trim(args(1))//'='
!       if (eps_row == j) stringa = trim(eps_type)//'='
!       write(unitFile,*) trim(stringa), values_to_replace(1)
!     endif
    
!     do i = 2, size(rows_number,1)
!       if (rows_number(i)==0) cycle
!       do j = maxval(rows_number(1:i-1))+1, rows_number(i)-1
!         read(unit_INP,'(A)') stringa
!         write(unitFile,'(A)') stringa
!       enddo
!       read(unit_INP,'(A)') stringa
!       call parse(stringa,'=',args)
!       stringa = trim(args(1))//'='
!       if (eps_row == j) stringa = trim(eps_type)//'='
!       write(unitFile,*) trim(stringa), values_to_replace(i)
!     enddo
    
!     !> Write all lines after last line to be replaced
!     ios = 0
!     do while(ios==0)
!       read(unit_INP,'(A)',iostat=ios) stringa
!       write(unitFile,'(A)') stringa
!     enddo
    
!     close(unitFile)

!   end subroutine CEAcopywrite


  subroutine CEAwrite(CEA,what,ONLYspecies)
    implicit none
    type(obj_CEA), intent(inout):: CEA
    character(len=*), intent(in):: what
    type(obj_CEA_species), optional:: ONLYspecies
    integer:: i, unitfile

    if (present(ONLYspecies)) CEA%only = .true.

    open(newunit=unitFile,file='CEAfile.inp')
    write(unitFile,*)'problem case=MOCA_was_here'
    write(unitfile,*)'rocket '//what
    write(unitfile,*)'p,bar=',CEA%pressure
    if (index(CEA%eps_type,'sub')>0) then
      write(unitfile,*)'sub,at/ae=',CEA%epsilon
    else
      write(unitfile,*)'sup,ae/at=',CEA%epsilon
    endif
    if (CEA%OF>1e-20) write(unitfile,*)'o/f=',CEA%OF
    write(unitfile,*)'react'
    do i = 1, CEA%nprop
      if (CEA%prop(i)%type=='F') then
        write(unitfile,*)'fuel='//adjustl(trim(CEA%prop(i)%name))//' wt=',CEA%prop(i)%wt,'t,k=',CEA%prop(i)%T
      else
        write(unitfile,*)'oxid='//adjustl(trim(CEA%prop(i)%name))//' wt=',CEA%prop(i)%wt,'t,k=',CEA%prop(i)%T
      endif
    if (CEA%prop(i)%more==' ') write(unitfile,*) CEA%prop(i)%more
    end do
    if (CEA%only) then
      write(unitfile,*)'only'
      do i = 1, ONLYspecies%n
        write(unitfile,*) ONLYspecies%name(i)
      enddo
    endif
    write(unitfile,*)'output'
    write(unitfile,*)'   siunits short massf transport'
    write(unitfile,*)'end'
    close(unitfile)

  end subroutine CEAwrite

end module CEA_module
