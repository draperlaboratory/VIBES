# Make sure this is bash.
if [ -z "${BASH_VERSION}" ]; then
    1>2& echo "No BASH_VERSION found. Use bash to run this script."
    exit 1
fi

# Define some paths
THIS_SCRIPT="${BASH_SOURCE[0]}"
THIS_DIR="$(cd "$(dirname "${THIS_SCRIPT}")" && pwd)"
COMMON_LIB_DIR="$(cd "${THIS_DIR}/../common-lib" && pwd)"
APT_PACKAGES="${COMMON_LIB_DIR}/packages.apt"

# Include relevant libraries/data.
. "${COMMON_LIB_DIR}/utils.bash"

# Usage message
usage () {
    report "USAGE: bash $(get_me) [OPTIONS]"
    report ""
    report "  Install the APT packages that vibes-tools needs."
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

report "- Installing APT packages:"
while IFS= read -r LINE; do
    report "  - ${LINE}"
done < "${APT_PACKAGES}"

# Do an APT update
sudo -E apt update 1>&2
APT_UPDATE_RESULT="${?}"
if [ "${APT_UPDATE_RESULT}" -ne "0" ]; then
    report "Unable to update APT repository information."
    report "Halting."
    report "Tried 'sudo -E apt update'."
    report "Got a non-zero exit code: ${APT_UPDATE_RESULT}."
    exit 1
else
    report "- APT repository information updated."
fi

# Install the APT packages
sudo -E < "${APT_PACKAGES}" xargs apt install -qq -yy 1>&2
APT_INSTALL_RESULT="${?}"
if [ "${APT_INSTALL_RESULT}" -ne "0" ]; then
    report "Unable to install APT packages."
    report "Halting."
    report "Tried to install APT packages."
    report "Got a non-zero exit code: ${APT_INSTALL_RESULT}."
    exit 1
else
    report "- APT packages installed."
fi

report "- Done" 
