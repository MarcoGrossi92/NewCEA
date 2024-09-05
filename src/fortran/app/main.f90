program CEAprogram
  implicit none
  external :: cea2
  character(200) :: filename, libpath, masterpath
  real(8) :: state(2,6), perfo(2,3)
  real(8) :: spec_frac(600)
  character(20) :: spec_name(600)

  ! Assign correct path depending on ENV variable
  ! call get_environment_variable('CEADIR',masterpath)
  ! libpath = masterpath//'/lib/thermo-transport/'
  libpath = '/Users/marcogrossi/Codici/myCEA/lib/thermo-transport/'

  read(*,*) filename
  call cea2 (libpath, filename, 1, spec_name, spec_frac, perfo, state)

end program CEAprogram