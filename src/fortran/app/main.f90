program CEAprogram
  use CEA_module, only: obj_CEA
  implicit none
  type(obj_CEA) :: CEA
  character(len=500) :: filename

  CEA%indx = 1
  read(*,*) filename
  call CEA%solve(filename)

end program CEAprogram