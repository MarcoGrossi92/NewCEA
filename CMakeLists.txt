# CMake project file for CEA
set(CMAKE_Fortran_COMPILER "gfortran")
project (CEA LANGUAGES Fortran)
set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin/)
add_library(NewCEA "src/fortran/lib/cea2.f" "src/fortran/lib/CEAinc.f90" "src/fortran/lib/obj_CEA.f90")
add_executable(FCEA2 "src/fortran/app/main.f90")
target_link_libraries(FCEA2 NewCEA)