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
  rm -rf bin build src/python/NewCEA/FCEA2* && mkdir -p build

  if [[ $BUILD == standalone ]]; then
    echo 
    echo -e "\033[0;32mStand-alone building \033[0m"
    echo
    Master=None
    # Determine the correct pip command
    if command -v pip3 &> /dev/null; then
        PIP_CMD="pip3"
    elif command -v pip &> /dev/null; then
        PIP_CMD="pip"
    else
        echo -e "\033[0;31mError: pip is not installed on this system.\033[0m"
        exit 1
    fi

    # Install the package using pip
    echo -e "\033[0;34mBuilding and installing NewCEA...\033[0m"
    $PIP_CMD install -e . $VERBOSE
    cd src/python/NewCEA
    mv FCEA2*.so FCEA2.so
    cd $DIR
    echo -e "\033[0;32mInstallation completed successfully.\033[0m"
  fi

  # Manual building
  # cd $DIR/build
  # cmake .. -DCMAKE_BUILD_TYPE=RELEASE
  # cmake --build .
  # cd $DIR
  # cd src/python/NewCEA
  # f2py -c -m FCEA2 ../../fortran/lib/CEAinc.f90 ../../fortran/lib/cea2.f
  # mv FCEA2* FCEA2.so

}

function compile () {
  mkdir -p build
  cd build
  cmake .. -DCMAKE_BUILD_TYPE=$TYPE
  make
}

VERBOSE=0
TYPE=0
SETVARS=0
BUILD=0

while test $# -gt 0; do
  if [ x"$1" == x"--" ]; then
    # detect argument termination
    shift
    break
  fi
  case $1 in

    --build | -b )
      shift
      BUILD=standalone
      ;;

    --compile | -c )
      shift
      TYPE="$1"
      ;;

    --setvars | -s )
      shift
      SETVARS=1
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
if [[ $# -eq 0 && "$BUILD" == "0" ]]; then
  usage
fi

if [ "$SETVARS" != "0" ]; then
  define_path
elif [[ "$BUILD" != "0" ]]; then
  define_path
  build_project
elif [[ "$TYPE" != "0" ]]; then
  compile
else
  usage
fi
