program CEAprogram
  implicit none
  external :: cea
  character(200) :: filename

  read(*,*) filename
  call cea ('./', filename, 1, 1)

end program CEAprogram