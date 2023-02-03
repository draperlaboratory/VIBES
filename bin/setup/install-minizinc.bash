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
. "${THIS_DIR}/paths.minizinc"

# Usage message
usage () {
    report "USAGE: bash $(get_me) [OPTIONS]"
    report ""
    report "  Install minizinc."
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

report "- Installing minizinc..."

# If minizinc isn't installed already, install it.
which minizinc > /dev/null 2>&1
if [ "${?}" -ne "0" ]; then
    CURRENT_DIR="$(pwd)"
    cd "${HOME}"
    if [ -d "${LOCAL_MINIZINC_DIR}" ]; then
        report "Can't install minizinc."
	report "Halting."
	report "Wanted to download minizinc to ${LOCAL_MINIZINC_DIR}."
	report "But ${LOCAL_MINIZINC_DIR} already exists."
	exit 1
    fi
    curl -L "${MINIZINC_URL}" --output minizinc.tgz
    tar zxf minizinc.tgz 2>&1
    cd "${CURRENT_DIR}"
    export PATH="${LOCAL_MINIZINC_BIN_DIR}":"${PATH}"
fi

# Make sure minizinc is installed
which minizinc > /dev/null 2>&1
if [ "${?}" -ne "0" ]; then
    report "Unable to find minizinc."
    report "Halting."
    report "Tried 'which minizinc' but got nothing."
    exit 1
else
    report "- Minizinc is installed: $(which minizinc)"
fi

report "- Done" 
