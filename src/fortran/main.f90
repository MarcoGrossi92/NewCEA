program CEAprogram
  implicit none
  external :: cea2
  character(200) :: filename, libpath, masterpath

  ! Assign correct path depending on ENV variable
  ! call get_environment_variable('CEADIR',masterpath)
  ! libpath = masterpath//'/lib/thermo-transport/'
  libpath = '/home/marco/Codici/myCEA/lib/thermo-transport/'

  read(*,*) filename
  call cea2 (libpath,'./', filename, 3)

end program CEAprogram