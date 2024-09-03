# CMake project file for CEA
set(CMAKE_Fortran_COMPILER "gfortran")
project (CEA LANGUAGES Fortran)
set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin/)
add_executable(FCEA "src/fortran/main.f90" "src/fortran/cea.f" "src/fortran/CEAmod.f90")