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
. "${THIS_DIR}/paths.bap"

# For installing bleeding edge BAP
BAP_REPO_NAME="bap-testing"
BAP_OPAM_URL='git+https://github.com/BinaryAnalysisPlatform/opam-repository#testing'

# Usage message
usage () {
    report "USAGE: bash $(get_me) [OPTIONS]"
    report ""
    report "  Install BAP."
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

report "- Installing BAP..."

# If bap isn't already installed, install it.
which bap > /dev/null 2>&1
if [ "${?}" -ne "0" ]; then
    opam repo add "${BAP_REPO_NAME}" "${BAP_OPAM_URL}" 1>&2
    if [ "${?}" -ne "0" ]; then
        report "Could not add the BAP opam repo."
	report "Halting."
	exit 1
    fi
    opam depext --install -y bap bap-elf 1>&2
    if [ "${?}" -ne "0" ]; then
        report "Failed to install BAP."
	report "Halting."
	exit 1
    fi
fi

# Make sure bap got installed
which bap > /dev/null 2>&1
if [ "${?}" -ne "0" ]; then
    report "Unable to find bap."
    report "Halting."
    report "Tried 'which bap' but got nothing."
    exit 1
else
    report "- BAP is installed: $(bap --version)"
fi

report "- Done"
