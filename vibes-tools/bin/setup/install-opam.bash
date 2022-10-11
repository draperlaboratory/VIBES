# Make sure this is bash.
if [ -z "${BASH_VERSION}" ]; then
    1>2& echo "No BASH_VERSION found. Use bash to run this script."
    exit 1
fi

# Define some paths
THIS_SCRIPT="${BASH_SOURCE[0]}"
THIS_DIR="$(cd "$(dirname "${THIS_SCRIPT}")" && pwd)"
COMMON_LIB_DIR="$(cd "${THIS_DIR}/../common-lib" && pwd)"
OPAM_PACKAGES="${COMMON_LIB_DIR}/packages.opam"

# Include relevant libraries/data.
. "${COMMON_LIB_DIR}/utils.bash"

# Usage message
usage () {
    report "USAGE: bash $(get_me) [OPTIONS]"
    report ""
    report "  Install the opam packages that vibes-tools needs."
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

report "- Installing opam packages:"
while IFS= read -r LINE; do
    report "  - ${LINE}"   
done < "${OPAM_PACKAGES}"

# Install the opam packages
< "${OPAM_PACKAGES}" xargs opam install -y 1>&2
OPAM_INSTALL_RESULT="${?}"
if [ "${OPAM_INSTALL_RESULT}" -ne "0" ]; then
    report "Unable to install opam packages."
    report "Halting."
    report "Tried to install opam packages."
    report "Got a non-zero exit code: ${OPAM_INSTALL_RESULT}."
    exit 1
else
    report "- Opam packages installed."
fi

report "- Done" 
