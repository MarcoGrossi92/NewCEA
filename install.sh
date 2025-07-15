#!/bin/bash

set -e  # Exit on any command failure
set -u  # Treat unset variables as an error

PROGRAM=$(basename "$0")
readonly DIR=$(pwd)
VERBOSE=false
BUILD_DIR="$DIR/build"
project=NewCEA

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

  setvars                   Set $project paths in environment variables

EOF
    exit 1
}


log() {
    if [ "$VERBOSE" = true ]; then
        # Bold and dim gray (ANSI escape: bold + color 90)
        echo -e "\033[1;90m$1\033[0m"
    fi
}

error() {
    # Bold red + [ERROR] tag, output to stderr
    echo -e "\033[1;31m[ERROR] $1\033[0m" >&2
}

task() {
    # Bold yellow + ==> tag, output to stdout
    echo -e "\033[1;38;5;186m==> $1\033[0m"
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
  log "RC file: $RCFILE"
  grep -v "NewCEA" $RCFILE > tmpfile && mv tmpfile $RCFILE
  echo 'source '$DIR'/.setvars.sh' >> $RCFILE
  source $RCFILE --force
}


# Create default CMakePresets.json if it doesn't exist
function write_presets() {
  FC=$(grep '^CMAKE_Fortran_COMPILER:FILEPATH=' "$BUILD_DIR/CMakeCache.txt" | cut -d= -f2-)

  cat <<EOF > CMakePresets.json
{
  "version": 3,
  "cmakeMinimumRequired": {
    "major": 3,
    "minor": 23
  },
  "configurePresets": [
    {
      "name": "default",
      "description": "Default preset",
      "binaryDir": "\${sourceDir}/build",
      "cacheVariables": {
        "CMAKE_BUILD_TYPE": "${BUILD_TYPE}",
        "CMAKE_Fortran_COMPILER": "${FC}",
      }
    }
  ]
}
EOF
  log "CMakePresets.json created with default settings."
}


# Default global values
COMMAND=""
COMPILERS=""
BUILD_TYPE="RELEASE"

# Define allowed options for each command using regular arrays
CMD=("build" "compile" "setvars")
CMD_OPTIONS_build=("--compilers")

# Parse global options
while getopts "hv-:" opt; do
    case "$opt" in
        -)
            case "$OPTARG" in
                verbose) VERBOSE=true ;;
                help) usage ;;
                *) error "Unknown global option '--$OPTARG'"; usage ;;
            esac
            ;;
        h) usage ;;
        v) VERBOSE=true ;;
        ?) error "Unknown global option '-$OPTARG'"; usage ;;
    esac
done
shift $((OPTIND -1))

# Ensure a command was provided
if [[ $# -eq 0 ]]; then
    error "No command provided!"
    usage
fi

COMMAND="$1"
# Check if the command is valid
if [[ ! " ${CMD[@]} " =~ " ${COMMAND} " ]]; then
    error "Unknown command '$COMMAND'"
    usage
fi
shift

# Parse command-specific options
while [[ $# -gt 0 ]]; do
    case "$1" in
        --compilers=*)
            [[ "$COMMAND" == "build" ]] || { error " --compilers is only valid for 'build' command"; exit 1; }
            COMPILERS="${1#*=}"
            ;;
        *)
            eval "opts=(\"\${CMD_OPTIONS_${COMMAND}[@]}\")"
            error "Unknown option '$1' for command '$COMMAND'. Valid options: ${opts[@]}"
            exit 1
            ;;
    esac
    shift
done

# Execute the selected command
case "$COMMAND" in
    build)
        # Determine the correct pip command
        if command -v pip3 &> /dev/null; then
            PIP_CMD="pip3"
        elif command -v pip &> /dev/null; then
            PIP_CMD="pip"
        else
            error -e "pip is not installed on this system."
            exit 1
        fi

        task "Compiling $project"
        if [[ $COMPILERS == "intel" ]]; then 
            export FC="ifx"
        elif [[ $COMPILERS == "gnu" ]]; then 
            export FC="gfortran"
        fi
        log "Build dir: $BUILD_DIR"
        log "Build type: $BUILD_TYPE"
        if [[ -z "${FC+x}" ]]; then
          log "Compilers not set. CMake will decide."
        else
          log "Compilers: FC=$FC"
        fi
        rm -rf $BUILD_DIR
        cmake -B $BUILD_DIR -DCMAKE_BUILD_TYPE=$BUILD_TYPE || exit 1
        cmake --build $BUILD_DIR || exit 1
        log "[OK] Compilation successful"

        task "Write CMakePresets.json"
        write_presets
        log "[OK] CMakePresets.json created"
  
        task "Building Python extension with f2py"
        cd src/python/NewCEA
        f2py -c -m FCEA2 ../../fortran/lib/CEAinc.f90 ../../fortran/lib/cea2.f
        mv FCEA2* FCEA2.so
        log "[OK] Python conversion of CEA successful"

        task "Installing NewCEA package"
        $PIP_CMD install -e . $VERBOSE
        cd $DIR
        log "[OK] NewCEA package installed"

        task "Defining environment variables"
        define_path
        log "[OK] Environment variables defined"
        ;;
    compile)
        task "Compiling $project using CMakePresets"
        cmake --preset default || exit 1
        cmake --build $BUILD_DIR || exit 1
        log "[OK] Fortran compilation successful"

        task "Building Python extension with f2py"
        cd src/python/NewCEA
        f2py -c -m FCEA2 ../../fortran/lib/CEAinc.f90 ../../fortran/lib/cea2.f
        mv FCEA2* FCEA2.so
        log "[OK] Python conversion of CEA successful"
        ;;
    setvars)
        task "Setting $project environment variables"
        define_path
        log "[OK] Environment variables defined"
        ;;
    *)
        error "Unknown command '$COMMAND'"
        usage
        ;;
esac
