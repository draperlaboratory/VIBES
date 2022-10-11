# Make sure this is bash.
if [ -z "${BASH_VERSION}" ]; then
    1>2& echo "No BASH_VERSION found. Use bash to run this script."
    exit 1
fi

# Define some paths
THIS_SCRIPT="${BASH_SOURCE[0]}"
THIS_DIR="$(cd "$(dirname "${THIS_SCRIPT}")" && pwd)"
COMMON_LIB_DIR="$(cd "${THIS_DIR}/../common-lib" && pwd)"

# Include relevant libraries/data.
. "${COMMON_LIB_DIR}/utils.bash"
. "${THIS_DIR}/paths.boolector"

# Usage message
usage () {
    report "USAGE: bash $(get_me) [OPTIONS]"
    report ""
    report "  Install boolector."
    report ""
    report "OPTIONS"
    report "  -h | --help  Print this help and exit"
}

# Parse command line arguments.
while (( "${#}" )); do
    case "${1}" in

        -h|--help)
            usage
	    exit 1
	    ;;

	*)
            report "Unrecognized argument: ${1}"
	    help_hint
	    exit 1
	    ;;

    esac
    shift
done

# Call clean_up before the script exits.
trap clean_up EXIT

report "- Installing boolector..."

# If boolector isn't already installed, install it.
which boolector > /dev/null 2>&1
if [ "${?}" -ne "0" ]; then
    CURRENT_DIR="$(pwd)"
    cd "${HOME}"
    if [ -d "${LOCAL_BOOLECTOR_DIR}" ]; then
        report "Can't install boolector."
	report "Halting."
	report "Wanted to clone repo to ${LOCAL_BOOLECTOR_DIR}."
	report "But ${LOCAL_BOOLECTOR_DIR} already exists."
	exit 1
    fi
    git clone "${BOOLECTOR_URL}"
    cd boolector
    ./contrib/setup-lingeling.sh
    ./contrib/setup-btor2tools.sh
    ./configure.sh && cd build && make
    cd "${CURRENT_DIR}"
    export PATH="${LOCAL_BOOLECTOR_BIN_DIR}":"${PATH}"
fi

# Make sure boolector is installed
which boolector > /dev/null 2>&1
if [ "${?}" -ne "0" ]; then
    report "Unable to find boolector."
    report "Halting."
    report "Tried 'which boolector' but got nothing."
    exit 1
else
    report "- Boolector is installed: $(which boolector)"
fi

report "- Done" 
