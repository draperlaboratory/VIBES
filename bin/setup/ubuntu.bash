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

# WP branch to install
WP_BRANCH=master

# Usage message
usage () {
    report "USAGE: bash $(get_me) [OPTIONS]"
    report ""
    report "  Install all dependencies for vibes-tools."
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

report "= INSTALLING ALL DEPENDENCIES..."

bash "${THIS_DIR}/install-apt.bash"
if [ "${?}" -ne "0" ]; then
    exit 1
fi

bash "${THIS_DIR}/install-opam.bash"
if [ "${?}" -ne "0" ]; then
    exit 1
fi

bash "${THIS_DIR}/install-bap.bash"
if [ "${?}" -ne "0" ]; then
    exit 1
fi

bash "${THIS_DIR}/install-bap-toolkit.bash"
if [ "${?}" -ne "0" ]; then
    exit 1
fi

bash "${THIS_DIR}/install-cbat.bash"
if [ "${?}" -ne "0" ]; then
    exit 1
fi

bash "${THIS_DIR}/install-minizinc.bash"
if [ "${?}" -ne "0" ]; then
    exit 1
fi

bash "${THIS_DIR}/install-boolector.bash"
if [ "${?}" -ne "0" ]; then
    exit 1
fi

report "= FINISHED INSTALLING DEPENDENCIES"
