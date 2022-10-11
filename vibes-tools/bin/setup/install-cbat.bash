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
. "${THIS_DIR}/paths.cbat"

# WP branch to install
WP_BRANCH=master

# Usage message
usage () {
    report "USAGE: bash $(get_me) [OPTIONS]"
    report ""
    report "  Install the CBAT tools."
    report ""
    report "OPTIONS"
    report "  -h | --help      Print this help and exit"
    report "  --wp-branch VAL  WP branch to install (default: main)"
}

# Parse command line arguments.
while (( "${#}" )); do
    case "${1}" in

        -h|--help)
            usage
	    exit 1
	    ;;

        --wp-branch)
            shift
            WP_BRANCH="${1}"
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

report "- Installing CBAT tools..."

# If WP isn't installed already, install it.
WP_IS_INSTALLED="$(bap list plugins | grep wp | grep 'weakest precondition')"
if [ -z "${WP_IS_INSTALLED}" ]; then
    CURRENT_DIR="$(pwd)"
    cd "${HOME}"
    if [ -d "${LOCAL_CBAT_DIR}" ]; then
        report "Can't install WP."
	report "Halting."
	report "Wanted to clone cbat_tools repo to '${LOCAL_CBAT_DIR}'."
	report "But '${LOCAL_CBAT_DIR}' already exists."
	exit 1
    fi
    git clone -b "${WP_BRANCH}" "${CBAT_URL}" 1>&2
    cd cbat_tools/wp
    make 1>&2
    cd "${CURRENT_DIR}"
fi

# Make sure WP got installed.
WP_IS_INSTALLED="$(bap list plugins | grep wp | grep 'weakest precondition')"
if [ -z "${WP_IS_INSTALLED}" ]; then
    report "Unable to find 'wp'."
    report "Halting."
    report "Grepped 'bap list plugins' for 'wp'."
    report "Only got this: '${WP_IS_INSTALLED}'."
    exit 1
else
    report "- WP is installed."
fi

report "- Done" 
