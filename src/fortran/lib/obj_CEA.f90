module CEAbox
  implicit none
  private
  !public:: CEAcopywrite

  type, public:: obj_propellant
    real(8):: T, wt
    character(len=15):: name
    character(len=1):: type
    character(len=100):: more
  end type obj_propellant

  type, public:: obj_species
    integer:: n
    real(8), dimension(:), allocatable:: massf, molef
    character(len=20), dimension(:), allocatable:: name
  end type obj_species

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
    type(obj_species):: species
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

  subroutine solve(self,filename)
    implicit none
    class(obj_CEA), intent(inout) :: self
    character(*), intent(inout) :: filename
    external :: cea2
    character(200) :: libpath
    real(8) :: state(2,6), perfo(2,3)
    real(8) :: spec_frac(600)
    character(20) :: spec_name(600)

    ! Assign correct path depending on ENV variable
    ! call get_environment_variable('CEADIR',masterpath)
    ! libpath = masterpath//'/lib/thermo-transport/'
    libpath = '/Users/marcogrossi/Codici/myCEA/lib/thermo-transport/'

    read(*,*) filename
    call cea2 (libpath, filename, 1, spec_name, spec_frac, perfo, state)

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
    self%SE%species%n = 89
    allocate(self%SE%species%name(1:self%SE%species%n))
    allocate(self%SE%species%massf(1:self%SE%species%n))
    self%SE%species%name = spec_name(1:self%SE%species%n)
    self%SE%species%massf = spec_frac(1:self%SE%species%n)

    call self%recompute_fractions()

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


!   subroutine CEAwrite(self,what,ONLYspecies)
!     implicit none
!     class(obj_CEA):: self
!     character(len=*), intent(in):: what
!     type(obj_species), optional:: ONLYspecies
!     integer:: i

!     if (present(ONLYspecies)) self%only = .true.

!     open(newunit=unitFile,file='CEAfile.inp')
!     write(unitFile,*)'problem case=MOCA_was_here'
!     write(unitfile,*)'rocket '//what
!     write(unitfile,*)'p,bar=',self%pressure
!     if (index(self%eps_type,'sub')>0) then
!       write(unitfile,*)'sub,at/ae=',self%epsilon
!     else
!       write(unitfile,*)'sup,ae/at=',self%epsilon
!     endif
!     if (self%OF>1e-20) write(unitfile,*)'o/f=',self%OF
!     write(unitfile,*)'react'
!     do i = 1, self%nprop
!       if (self%prop(i)%type=='F') then
!         write(unitfile,*)'fuel='//adjustl(trim(self%prop(i)%name))//' wt=',self%prop(i)%wt,'t,k=',self%prop(i)%T
!       else
!         write(unitfile,*)'oxid='//adjustl(trim(self%prop(i)%name))//' wt=',self%prop(i)%wt,'t,k=',self%prop(i)%T
!       endif
!     if (self%prop(i)%more==' ') write(unitfile,*) self%prop(i)%more
!     end do
!     if (self%only) then
!       write(unitfile,*)'only'
!       do i = 1, ONLYspecies%n
!         write(unitfile,*) ONLYspecies%name(i)
!       enddo
!     endif
!     write(unitfile,*)'output'
!     write(unitfile,*)'   siunits short massf transport'
!     write(unitfile,*)'end'
!     close(unitfile)

!   end subroutine CEAwrite


!   subroutine CEAread(self,outfile,p,T,cp,cp_eq,gamma,w,Mach,species,h,h0,a,ivac,what)
!     implicit none
!     class(obj_CEA), intent(inout)          :: self
!     character(len=*), intent(in)           :: outfile
!     logical, intent(in), optional          :: p, T, gamma, cp, cp_eq, w, Mach, species, h, h0, a, ivac
!     character(len=*), intent(in), optional :: what
!     real, parameter :: cpCondP=1.5977e+03
!     real:: values(101), cpval, yCondP
!     real:: dummy(100), dummy_cond(100)
!     integer:: round, f, found_cp
!     integer:: icount, ierr, icond, i
!     character(len=20):: args(2)
!     character(len=300):: search_try, dummy_s(100), dummy_scond(100), stringa
!     logical:: frozen_present, frozen_reading, equilibrium_present, CondP_present, force_equilibrium_read

!     !call execute_command_line ("sed -i 's/\//div/gI' "//trim(adjustl(outfile)))
!     call execute_command_line ("perl -i -pe's/\//div/g' "//trim(adjustl(outfile)))

!     !> Reading from CEA output
!     open(newunit=Unit_OUT,file=trim(adjustl(outfile)))
!     round = 2
!     CondP_present = .false.

!     !> Look for frozen solution
!     frozen_present = .true.
!     do while(index(search_try,'THEORETICAL ROCKET PERFORMANCE ASSUMING FROZEN COMPOSITION')==0)
!       read(Unit_OUT,'(A)',iostat=ios) search_try
!       if (ios<0) then
!         frozen_present = .false.
!         round = round-1
!         exit
!       endif
!     end do
!     rewind(Unit_OUT)

!     equilibrium_present = .true.
!     !> Look for equilibrium solution
!     do while(index(search_try,'THEORETICAL ROCKET PERFORMANCE ASSUMING EQUILIBRIUM')==0)
!       read(Unit_OUT,'(A)',iostat=ios) search_try
!       if (ios<0) then
!         equilibrium_present = .false.
!         round = round-1
!         exit
!       endif
!     end do
!     rewind(Unit_OUT)

!     force_equilibrium_read = .false.
!     if (present(what)) then
!       if (what=='equilibrium') force_equilibrium_read=.true.
!     endif

!     if (round<1) error stop ("CEA output file empty")
!     if (round == 2 .and. force_equilibrium_read) round = 1

!     do f = 1, round
!       search_try = 'via'

!       if (frozen_present .and. f==1 .and. .not.force_equilibrium_read) then
!         do while(index(search_try,'THEORETICAL ROCKET PERFORMANCE ASSUMING FROZEN COMPOSITION')==0)
!           read(Unit_OUT,'(A)',iostat=ios) search_try
!         end do
!         frozen_reading = .true.
!         ! write(*,*)' Reading CEA frozen output'
!       else
!         do while(index(search_try,'THEORETICAL ROCKET PERFORMANCE ASSUMING EQUILIBRIUM')==0)
!           read(Unit_OUT,'(A)',iostat=ios) search_try
!         end do
!         frozen_reading = .false.
!         ! write(*,*)' Reading CEA equilibrium output'
!       endif

!       !> Mass Fractions
!       if (.not.frozen_reading) then
!         dummy = 0.0; i = 0; icond = 0; yCondP = 0
!         do while(index(search_try,'MASS FRACTIONS')==0)
!           read(Unit_OUT,'(A)',iostat=ios) search_try
!           if (ios.ne.0) exit
!         end do
!         read(Unit_OUT,'(A)',iostat=ios) search_try
!         do
!           read(Unit_OUT,'(A)',iostat=ios) search_try
!           if (adjustl(trim(search_try))=='') exit
!           call getvals(search_try,values,icount,ierr)

!           stringa = search_try(1:18)
!           if (stringa(2:2)=='*') stringa = stringa(3:18)
!           if (index(stringa,'(L)')/=0 .or. index(stringa,'(cr)')/=0) then
!             icond = icond+1
!             CondP_present = .true.
!             call parse(stringa,'(',args)
!             stringa = args(1)
!             dummy_scond(icond) = args(1)
!             dummy_cond(icond) = values(self%indx)
!             yCondP = yCondP+values(self%indx)
!             if (self%OG) cycle
!           endif
!           i = i+1
!           dummy(i) = values(self%indx)
!           dummy_s(i) = adjustl(stringa)
!         end do
        
!         if (present(species)) then
!           if (self%OG) dummy = dummy/(1-yCondP)
!           self%SE%species%n = i
!           if (.not.allocated(self%SE%species%massf)) allocate(self%SE%species%massf(1:i))
!           self%SE%species%massf = dummy(1:i)
!           if (.not.allocated(self%SE%species%name)) allocate(self%SE%species%name(1:i))
!           self%SE%species%name = dummy_s(1:i)
!         endif
        
!         ! condspecies%n = icond
!         ! allocate(condspecies%massf(1:icond))
!         ! condspecies%massf = dummy_cond(1:icond)
!         ! allocate(condspecies%name(1:icond))
!         ! condspecies%name = dummy_scond(1:icond)
                
!         rewind(Unit_OUT)
!       endif

!       !> Exit Pressure
!       do while(index(search_try,'P, BAR')==0)
!         read(Unit_OUT,'(A)',iostat=ios) search_try
!         if(ios.ne.0) exit
!       end do
!       call getvals(search_try,values,icount,ierr)
!       ! write(*,*)'VALUES=',values(:icount)
!       if (present(p)) then
!         if (frozen_reading) then
!           self%FE%Pressure = values(self%indx)*1d+5
!         else
!           self%SE%pressure = values(self%indx)*1d+5
!         endif
!       endif

!       !> Exit Temperature
!       read(Unit_OUT,'(A)',iostat=ios) search_try
!       call getvals(search_try,values,icount,ierr)
!       ! write(*,*)'VALUES=',values(:icount)
!       if (present(T)) then
!         if (frozen_reading) then
!           self%FE%temperature = values(self%indx)
!         else
!           self%SE%temperature = values(self%indx)
!         endif
!       endif

!       !> Exit enthalpy
!       do while(index(search_try,'H, KJ')==0)
!         read(Unit_OUT,'(A)',iostat=ios) search_try
!         if(ios.ne.0) exit
!       end do
!       call getvals(search_try,values,icount,ierr)
!       ! write(*,*)'VALUES=',values(:icount)
!       if (present(h) .or. present(h0)) then
!         if (frozen_reading) then
!           self%FE%h = values(self%indx)*1d+3
!         else
!           self%SE%h = values(self%indx)*1d+3
!         endif
!       endif
          
!       !> Exit molecular weight
!       do while(index(search_try,'1divn')==0)
!         read(Unit_OUT,'(A)',iostat=ios) search_try
!         if(ios.ne.0) exit
!       end do
!       call getvals(search_try,values,icount,ierr)
!       ! write(*,*)'VALUES=',values(:icount)
!       if (present(w)) then
!         if (frozen_reading) then
!           self%FE%w = values(self%indx)
!         else
!           self%SE%w = values(self%indx)
!         endif
!       endif

!       !> Exit cp equilibrium (EQUILIBRIUM)
!       if (.not.frozen_reading .and. present(cp_eq)) then
!         do while(index(search_try,'Cp, KJ')==0)
!           read(Unit_OUT,'(A)',iostat=ios) search_try
!           if(ios.ne.0) exit
!         end do
!         call getvals(search_try,values,icount,ierr)
!         ! write(*,*)'VALUES=',values(:icount)
!         cpval = values(self%indx)*1000
!         self%SE%cpeq = cpval
!       end if

!       !> Exit cp and gamma (FROZEN)
!       if (frozen_reading) then
!         do while(index(search_try,'Cp, KJ')==0)
!           read(Unit_OUT,'(A)',iostat=ios) search_try
!           if(ios.ne.0) exit
!         end do
!         call getvals(search_try,values,icount,ierr)
!         ! write(*,*)'VALUES=',values(:icount)
!         cpval = values(self%indx)*1000
!         if (present(cp)) self%FE%cp = cpval
!         if (present(gamma)) self%FE%gamma = cpval/(cpval-Runi/self%FE%w)
!       end if

!       !> Exit speed of sound
!       do while(index(search_try,'SON VEL')==0)
!        read(Unit_OUT,'(A)',iostat=ios) search_try
!        if(ios.ne.0) exit
!       end do
!       call getvals(search_try,values,icount,ierr)
!       ! write(*,*)'VALUES=',values(:icount)
!       if (present(a) .or. present(h0)) then
!         if (frozen_reading) then
!           self%FE%a = values(self%indx)
!         else
!           self%SE%a = values(self%indx)
!         endif
!       endif

!       !> Exit Mach Number
!       do while(index(search_try,'MACH')==0)
!        read(Unit_OUT,'(A)',iostat=ios) search_try
!        if(ios.ne.0) exit
!       end do
!       call getvals(search_try,values,icount,ierr)
!       ! write(*,*)'VALUES=',values(:icount)
!       if (present(Mach) .or. present(h0)) then
!         if (frozen_reading) then
!           self%FE%Mach = values(self%indx)
!         else
!           self%SE%Mach = values(self%indx)
!         endif
!       endif

!       !> Ivac
!       do while(index(search_try,'Ivac')==0)
!        read(Unit_OUT,'(A)',iostat=ios) search_try
!        if(ios.ne.0) exit
!       end do
!       call getvals(search_try,values,icount,ierr)
!       if (present(ivac)) then
!         if (frozen_reading) then
!           self%FE%ivac = values(icount)
!         else
!           self%SE%ivac = values(icount)
!         endif
!       endif
    
!       !> Exit cp (REACTIVE)
!       rewind(Unit_OUT)
!       if (.not.frozen_reading .and. .not.present(cp_eq)) then
!         found_cp = 0
!         do while(found_cp<3)
!           do while(index(search_try,'Cp, KJ')==0)
!             read(Unit_OUT,'(A)',iostat=ios) search_try
!             if(ios.ne.0) exit
!           end do
!           found_cp = found_cp+1
!           if(found_cp<=2)search_try = 'restart'
!         end do
!         call getvals(search_try,values,icount,ierr)
!         ! write(*,*)'VALUES=',values(:icount)
!         cpval = values(self%indx)*1000
!         cpval = cpval*(1-yCondP)+cpCondP*yCondP
!         if (present(cp)) self%SE%cp = cpval
!         if (present(gamma)) self%SE%gamma = cpval/(cpval-Runi/self%SE%w)
!       endif

!       if (present(h0)) then
!         if (frozen_reading) then
!           self%FE%h0 = self%FE%h+0.5d0*(self%FE%Mach*self%FE%a)**2.d0
!         else
!           self%SE%h0 = self%SE%h+0.5d0*(self%SE%Mach*self%SE%a)**2.d0
!         endif
!       endif

!       rewind(Unit_OUT)

!     end do

!     close(Unit_OUT)

!   end subroutine CEAread


!   !> Read CEA section of the input INI file (FiNeR library)
!   subroutine read_CEA_input(section_name,INfile,INsource,CEAobj_)
!     use finer, only: file_ini
!     implicit none
!     character(len=*), intent(in)           :: section_name
!     character(len=*), intent(in), optional :: INfile
!     type(file_ini), intent(in), optional   :: INsource
!     type(obj_CEA), intent(out)             :: CEAobj_
!     character(len=:), allocatable  :: item(:) !< List of section options name/value pairs.
!     character(len=4)               :: ind
!     character(len=99)              :: wholestring
!     character(len=15), allocatable :: stringa(:)
!     type(file_ini)                 :: fini
!     integer                        :: error, i

!     if (present(INfile)) call fini%load(filename=INfile)
!     if (present(INsource)) fini = INsource

!     call fini%get(section_name=section_name, option_name='section', val=CEAobj_%indx, error=error)
!     if (error/=0) then
!       CEAobj_%indx = 1 !< Default value = 1 -> chamber
!     endif

!     !> CEA-file
!     call fini%get(section_name=section_name, option_name='CEAfile', val=CEAfile, error=error)
!     if (error==0) then
!       CEAobj_%input_mode='file'
!       return
!     endif

!     !> CEA-data 

!     !> Look for pressure, of, epsilon
!     call fini%get(section_name=section_name, option_name='pressure', val=CEAobj_%pressure, error=error)
!     ! if (error==0) print *, ' pressure has values: ', CEAobj_%pressure
!     call fini%get(section_name=section_name, option_name='of', val=CEAobj_%of, error=error)
!     ! if (error==0) print *, ' of has values: ', CEAobj_%of
!     call fini%get(section_name=section_name, option_name='epsilon', val=CEAobj_%epsilon, error=error)
!     if (error/=0) CEAobj_%epsilon = 1
  
!     !> Look for propellants
!     CEAobj_%nprop = 0
!     do while(fini%loop(section_name=section_name, option_pairs=item))
!       ! write(*,"(A)") '  '//trim(item(1))//' = '//trim(item(2))
!       if (index(item(1),'prop')==0) cycle
!       CEAobj_%nprop = CEAobj_%nprop+1
!     enddo
!     allocate(CEAobj_%prop(1:CEAobj_%nprop))

!     !> Look for propellants properties
!     do i = 1, CEAobj_%nprop
!       write(ind,'(I4)') i
!       if (.not.allocated(stringa)) &
!           allocate(stringa(1:fini%count_values(section_name=section_name, option_name='prop'//adjustl(ind))))
!       call fini%get(section_name=section_name, option_name='prop'//adjustl(ind), val=wholestring, error=error)
!       call parse(wholestring,' ',stringa)
!       CEAobj_%prop(i)%type = stringa(1)!; print *, CEAobj_%prop(i)%type
!       CEAobj_%prop(i)%name = stringa(2)!; print *, CEAobj_%prop(i)%name
!       read(stringa(3),'(F10.5)') CEAobj_%prop(i)%wt!; print *, prop(i)%wt
!       read(stringa(4),'(F10.5)') CEAobj_%prop(i)%T!; print *, prop(i)%T
!     enddo

!   end subroutine read_CEA_input


end module CEAbox
