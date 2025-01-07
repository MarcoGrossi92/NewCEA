#!/bin/bash -
#===============================================================================
#
#          FILE: intstall.sh
#
#         USAGE: run "./install.sh [options]" from NewCEA master directory
#
#   DESCRIPTION: A utility script that builds NewCEA project
#===============================================================================

# DEBUGGING
set -e
set -C # noclobber

# INTERNAL VARIABLES AND INITIALIZATIONS
readonly PROJECT="NewCEA"
readonly DIR=$(pwd)
readonly PROGRAM=`basename "$0"`

function usage () {
    echo "Install script of $PROJECT"
    echo "Usage:"
    echo
    echo "$PROGRAM --help|-?"
    echo "    Print this usage output and exit"
    echo
    echo "$PROGRAM --build  |-b"
    echo "    Build the project and libraries via setup.py"
    echo
    echo "$PROGRAM --compile|-c <build>"
    echo "    Compile the Fortran sources (<build> = RELEASE,DEBUG,TESTING)"
    echo
    echo "$PROGRAM --full-compile|-f <build>"
    echo "    Compile the Fortran sources and update the python extension (<build> = RELEASE,DEBUG,TESTING)"
    echo
    echo "$PROGRAM --setvars|-s"
    echo "    Set the project paths in the environment variables"
    echo
}


function define_path () {
  rm -f .setvars.sh
  echo 'export NewCEADIR='$DIR >> .setvars.sh
  echo 'export PYTHONPATH='$DIR'/src/python/NewCEA/:$PYTHONPATH' >> .setvars.sh
  if [[ $SHELL == *"zsh"* ]]; then
    RCFILE=$HOME/.zshrc
  elif [[ $SHELL == *"bash"* ]]; then
    RCFILE=$HOME/.bashrc
  fi
  grep -v "NewCEA" $RCFILE > tmpfile && mv tmpfile $RCFILE
  echo 'source '$DIR'/.setvars.sh' >> $RCFILE
  source $RCFILE --force
}

function compile () {
  echo -e "\033[0;34mBuilding Fortran sources via CMake...\033[0m"
  mkdir -p $DIR/build
  cd $DIR/build
  cmake .. -DCMAKE_BUILD_TYPE=$TYPE
  make
}

function full_compile () {
  rm -rf src/python/NewCEA/FCEA2*
  compile

  echo -e "\033[0;34mBuilding Python extension with f2py...\033[0m"
  cd $DIR
  cd src/python/NewCEA
  f2py -c -m FCEA2 ../../fortran/lib/CEAinc.f90 ../../fortran/lib/cea2.f
  mv FCEA2* FCEA2.so
}

function build_project () {
  rm -rf bin build

  echo 
  echo -e "\033[0;32mNewCEA building \033[0m"
  echo
  # Determine the correct pip command
  if command -v pip3 &> /dev/null; then
      PIP_CMD="pip3"
  elif command -v pip &> /dev/null; then
      PIP_CMD="pip"
  else
      echo -e "\033[0;31mError: pip is not installed on this system.\033[0m"
      exit 1
  fi

  full_compile

  # Install the package using pip
  echo -e "\033[0;34mInstalling NewCEA package...\033[0m"
  $PIP_CMD install -e . $VERBOSE
  cd $DIR
  echo -e "\033[0;32mInstallation completed successfully.\033[0m"

}

VERBOSE=F
TYPE=F
SETVARS=F
COMPILE=F
FCOMPILE=F
BUILD=F

while test $# -gt 0; do
  if [ x"$1" == x"--" ]; then
    # detect argument termination
    shift
    break
  fi
  case $1 in

    --build | -b )
      shift
      BUILD=T
      TYPE="RELEASE"
      ;;

    --compile | -c )
      shift
      COMPILE=T
      TYPE="$1"
      ;;

    --full-compile | -f )
      shift
      FCOMPILE=T
      TYPE="$1"
      ;;

    --setvars | -s )
      shift
      SETVARS=T
      ;;

    --verbose | -v )
      shift
      VERBOSE="--verbose"
      ;;

    -? | --help )
      usage
      exit
      ;;

    -* )
      echo "Unrecognized option: $1" >&2
      usage
      exit $E_BAD_OPTION
      ;;

    * )
      break
      ;;
  esac
done

# PROCESS COMMAND-LINE ARGUMENTS
if [[ $# -eq 0 && "$BUILD" == "F" ]]; then
  usage
fi

if [ "$SETVARS" != "F" ]; then
  define_path
elif [[ "$BUILD" != "F" ]]; then
  #define_path
  build_project
elif [[ "$COMPILE" != "F" ]]; then
  compile
elif [[ "$FCOMPILE" != "F" ]]; then
  full_compile
else
  usage
fi
