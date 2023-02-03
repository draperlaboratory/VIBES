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
. "${THIS_DIR}/paths.bap-toolkit"

# Usage message
usage () {
    report "USAGE: bash $(get_me) [OPTIONS]"
    report ""
    report "  Install bap-toolkit."
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

report "- Installing bap-toolkit..."

# If the bap-toolkit isn't already installed, install it.
BAP_TOOLKIT_IS_INSTALLED="$(bap list plugins | grep must-check-value)"
if [ -z "${BAP_TOOLKIT_IS_INSTALLED}" ]; then
    CURRENT_DIR="$(pwd)"
    cd "${HOME}"
    if [ -d "${LOCAL_BAP_TOOLKIT_DIR}" ]; then
        report "Can't install bap-toolkit."
	report "Halting."
	report "Wanted to clone bap-toolkit repo to '${LOCAL_BAP_TOOLKIT_DIR}'."
	report "But '${LOCAL_BAP_TOOLKIT_DIR}' already exists."
	exit 1
    fi
    git clone "${BAP_TOOLKIT_URL}" 1>&2
    if [ "${?}" -ne "0" ]; then
        report "Couldn't clone '${BAP_TOOLKIT_URL}'."
	report "Halting."
	exit 1
    fi
    cd bap-toolkit
    make 1>&2
    if [ "${?}" -ne "0" ]; then
        report "Couldn't build bap-toolkit."
	report "Halting."
	exit 1
    fi
    make install 1>&2
    if [ "${?}" -ne "0" ]; then
        report "Couldn't install bap-toolkit."
	report "Halting."
	exit 1
    fi
fi

# Make sure bap-toolkit got installed
BAP_TOOLKIT_IS_INSTALLED="$(bap list plugins | grep must-check-value)"
if [ -z "${BAP_TOOLKIT_IS_INSTALLED}" ]; then
    report "Can't find bap-toolkit."
    report "Halting."
    report "Grepped 'bap list plugins' for 'must-check-value'."
    report "But only got this: '${BAP_TOOLKIT_IS_INSTALLED}'."
    exit 1
else
    report "- bap-toolkit is installed."
fi

report "- Done"
