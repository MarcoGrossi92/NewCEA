#!/bin/bash

set -e  # Exit on any command failure
set -u  # Treat unset variables as an error

PROGRAM=$(basename "$0")
readonly DIR=$(pwd)
VERBOSE=false

function usage() {
    cat <<EOF

Install script for NewCEA

Usage:
  $PROGRAM [GLOBAL_OPTIONS] COMMAND [COMMAND_OPTIONS]

Global Options:
  -h       , --help         Show this help message and exit
  -v       , --verbose      Enable verbose output

Commands:
  build                     Perform a full build

  compile                   Compile the program using the CMakePresets file

  setvars                   Set project paths in environment variables

EOF
    exit 1
}


function log() {
    if [ "$VERBOSE" = true ]; then
        echo "$1"
    fi
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


# Default global values
COMMAND=""
BUILD_TYPE="RELEASE"

# Parse options with getopts
while getopts "hv:-:" opt; do
    case "$opt" in
        -)
            case "$OPTARG" in
                verbose) VERBOSE=true ;;
                help) usage ;;
                *) echo "Error: Unknown global option '--$OPTARG'"; usage ;;
            esac
            ;;
        h) usage ;;
        v) VERBOSE=true ;;
        *) echo "Error: Unknown global option '-$opt'"; usage ;;
    esac
done
shift $((OPTIND -1))

# Ensure a command was provided
if [[ $# -eq 0 ]]; then
    echo "Error: No command provided!"
    usage
fi

COMMAND="$1"
shift

# Execute the selected command
case "$COMMAND" in
    build)
        rm -rf bin build
        # Determine the correct pip command
        if command -v pip3 &> /dev/null; then
            PIP_CMD="pip3"
        elif command -v pip &> /dev/null; then
            PIP_CMD="pip"
        else
            echo -e "\033[0;31mError: pip is not installed on this system.\033[0m"
            exit 1
        fi

        # Compile the project
        cmake -B build -DCMAKE_BUILD_TYPE=$BUILD_TYPE || exit 1
        cmake --build build || exit 1

        # Ccompile the dynamic lib
        echo -e "\033[0;34mBuilding Python extension with f2py...\033[0m"
        cd src/python/NewCEA
        f2py -c -m FCEA2 ../../fortran/lib/CEAinc.f90 ../../fortran/lib/cea2.f
        mv FCEA2* FCEA2.so

        # Install the package using pip
        echo -e "\033[0;34mInstalling NewCEA package...\033[0m"
        $PIP_CMD install -e . $VERBOSE
        cd $DIR
        echo -e "\033[0;32mInstallation completed successfully.\033[0m"
        ;;
    compile)
        # Configure and build using the default preset
        cmake --build build || exit 1

        # Recompile the dynamic lib
        echo -e "\033[0;34mBuilding Python extension with f2py...\033[0m"
        cd src/python/NewCEA
        f2py -c -m FCEA2 ../../fortran/lib/CEAinc.f90 ../../fortran/lib/cea2.f
        mv FCEA2* FCEA2.so
        ;;
    setvars)
        log "Setting project environment variables"
        define_path
        ;;
    *)
        echo "Error: Unknown command '$COMMAND'"
        usage
        ;;
esac
