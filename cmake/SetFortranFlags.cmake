######################################################
# Determine and set the Fortran compiler flags we want 
######################################################

####################################################################
# Make sure that the default build type is Release if not specified.
####################################################################
INCLUDE(${CMAKE_MODULE_PATH}/SetCompileFlag.cmake)

# Make sure the build type is uppercase
STRING(TOUPPER "${CMAKE_BUILD_TYPE}" BT)

IF(BT STREQUAL "Release")
    SET(CMAKE_BUILD_TYPE Release CACHE STRING
      "Choose the type of build, options are Debug, Release, or Testing."
      FORCE)
ELSEIF(BT STREQUAL "Debug")
    SET (CMAKE_BUILD_TYPE Debug CACHE STRING
      "Choose the type of build, options are Debug, Release, or Testing."
      FORCE)
ELSEIF(BT STREQUAL "Testing")
    SET (CMAKE_BUILD_TYPE Testing CACHE STRING
      "Choose the type of build, options are Debug, Release, or Testing."
      FORCE)
ELSEIF(NOT BT)
    SET(CMAKE_BUILD_TYPE Release CACHE STRING
      "Choose the type of build, options are Debug, Release, or Testing."
      FORCE)
    MESSAGE(STATUS "CMAKE_BUILD_TYPE not given, defaulting to Release")
ELSE()
    MESSAGE(FATAL_ERROR "CMAKE_BUILD_TYPE not valid, choices are Debug, Release, or Testing")
ENDIF(BT STREQUAL "Release")

#########################################################
# If the compiler flags have already been set, return now
#########################################################

IF(CMAKE_Fortran_FLAGS_Release AND CMAKE_Fortran_FLAGS_Testing AND CMAKE_Fortran_FLAGS_Debug)
    RETURN ()
ENDIF(CMAKE_Fortran_FLAGS_Release AND CMAKE_Fortran_FLAGS_Testing AND CMAKE_Fortran_FLAGS_Debug)

########################################################################
# Determine the appropriate flags for this compiler for each build type.
# For each option type, a list of possible flags is given that work
# for various compilers.  The first flag that works is chosen.
# If none of the flags work, nothing is added (unless the REQUIRED 
# flag is given in the call).  This way unknown compiles are supported.
#######################################################################

#####################
### GENERAL FLAGS ###
#####################

# Don't add underscores in symbols for C-compatability
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}"
                 Fortran "-fno-underscoring")

# There is some bug where -march=native doesn't work on Mac
IF(APPLE)
    SET(GNUNATIVE "-mtune=native")
ELSE()
    SET(GNUNATIVE "-march=native")
ENDIF()
# Optimize for the host's architecture
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}"
                 Fortran "-xHost"        # Intel
                         "/QxHost"       # Intel Windows
                         ${GNUNATIVE}    # GNU
                         "-ta=host"      # Portland Group
                )
# Add preprocessor flag
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS}"
                 Fortran "-cpp"        # Intel or GNU
                )

###################
### Debug FLAGS ###
###################

# NOTE: debugging symbols (-g or /debug:full) are already on by default

# Disable optimizations
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_Debug "${CMAKE_Fortran_FLAGS_Debug}"
                 Fortran REQUIRED "-O0" # All compilers not on Windows
                                  "/Od" # Intel Windows
                )

# Turn on all warnings 
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_Debug "${CMAKE_Fortran_FLAGS_Debug}"
                 Fortran "-warn all" # Intel
                         "/warn:all" # Intel Windows
                         "-Wall"     # GNU
                                     # Portland Group (on by default)
                )

# Traceback
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_Debug "${CMAKE_Fortran_FLAGS_Debug}"
                 Fortran "-traceback"   # Intel/Portland Group
                         "/traceback"   # Intel Windows
                         "-fbacktrace"  # GNU (gfortran)
                         "-ftrace=full" # GNU (g95)
                )

# Check array bounds
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_Debug "${CMAKE_Fortran_FLAGS_Debug}"
                 Fortran "-check bounds"  # Intel
                         "/check:bounds"  # Intel Windows
                         "-fcheck=bounds" # GNU (New style)
                         "-fbounds-check" # GNU (Old style)
                         "-Mbounds"       # Portland Group
                )

#####################
### Testing FLAGS ###
#####################

# Optimizations
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_Testing "${CMAKE_Fortran_FLAGS_Testing}"
                 Fortran REQUIRED "-O2" # All compilers not on Windows
                                  "/O2" # Intel Windows
                )

#####################
### Release FLAGS ###
#####################

# NOTE: agressive optimizations (-O3) are already turned on by default

# Unroll loops
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_Release "${CMAKE_Fortran_FLAGS_Release}"
                 Fortran "-funroll-loops" # GNU
                         "-unroll"        # Intel
                         "/unroll"        # Intel Windows
                         "-Munroll"       # Portland Group
                )

# Inline functions
SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_Release "${CMAKE_Fortran_FLAGS_Release}"
                 Fortran "-inline"            # Intel
                         "/Qinline"           # Intel Windows
                         "-finline-functions" # GNU
                         "-Minline"           # Portland Group
                )

# Interprocedural (link-time) optimizations
# SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_Release "${CMAKE_Fortran_FLAGS_Release}"
#                  Fortran "-ipo"     # Intel
#                          "/Qipo"    # Intel Windows
#                          "-flto"    # GNU
#                          "-Mipa"    # Portland Group
#                 )

# Single-file optimizations
#SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_Release "${CMAKE_Fortran_FLAGS_Release}"
#                 Fortran "-ip"  # Intel
#                         "/Qip" # Intel Windows
#                )

# Vectorize code
#SET_COMPILE_FLAG(CMAKE_Fortran_FLAGS_Release "${CMAKE_Fortran_FLAGS_Release}"
#                 Fortran "-vec-report0"  # Intel
#                         "/Qvec-report0" # Intel Windows
#                         "-Mvect"        # Portland Group
#                )
