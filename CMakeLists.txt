# CMake project file for NewCEA

cmake_minimum_required(VERSION 3.20)

project (NewCEA LANGUAGES Fortran)

# Add our local modlues to the module path
set (CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/")
# This INCLUDE statement executes code that sets the compile flags for DEBUG,
# RELEASE, and TESTING. You should  review this file and make sure the flags 
# are to your liking.
include (${CMAKE_MODULE_PATH}/SetFortranFlags.cmake) 
set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin/)
set (CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/modules")

# Build library
add_library(NewCEA "src/fortran/lib/cea2.f" "src/fortran/lib/CEAinc.f90" "src/fortran/lib/obj_CEA.f90")

# Build application (only if build has not been called by another program)
if(CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR)
  add_executable(FCEA2 "src/fortran/app/main.f90")
  target_link_libraries(FCEA2 NewCEA)
endif()