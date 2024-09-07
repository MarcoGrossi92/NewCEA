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
    echo "$PROGRAM --build  |-b <master>"
    echo "    Build the project and libraries via CMake (<master> = standalone,chimera,hydra)"
    echo
    echo "$PROGRAM --compile|-c <build>"
    echo "    Compile a program with a build type (<build> = RELEASE,DEBUG,TESTING)"
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

function build_project () {
  rm -rf bin build && mkdir -p build
  if [[ $BUILD == standalone ]]; then
    echo 
    echo -e "\033[0;32mStand-alone building \033[0m"
    echo
    Master=None
  fi
  cd $DIR/build
  cmake .. -DCMAKE_BUILD_TYPE=RELEASE
  cmake --build .

  # python setup.py sdist bdist_wheel
  # pip install -e .
}

function compile () {
  mkdir -p build
  cd build
  cmake .. -DCMAKE_BUILD_TYPE=$TYPE
  make
}

EXE=0
TYPE=0
SETVARS=0
BUILD=0

# RETURN VALUES/EXIT STATUS CODES
readonly E_BAD_OPTION=254

# PROCESS COMMAND-LINE ARGUMENTS
if [ $# -eq 0 ]; then
  usage
  exit 0
fi

while test $# -gt 0; do
  if [ x"$1" == x"--" ]; then
    # detect argument termination
    shift
    break
  fi
  case $1 in

    --build | -b )
      shift
      if (( $# > 0 )); then
        BUILD=$1
      else
        BUILD=standalone
      fi
      ;;

    --compile | -c )
      shift
      TYPE="$1"
      ;;

    --setvars | -s )
      shift
      SETVARS=1
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

if [ "$SETVARS" != "0" ]; then
  define_path
elif [[ "$BUILD" != "0" ]]; then
  define_path
  build_project
elif [[ "$EXE" != "0" ]]; then
  compile
else
  usage
fi
