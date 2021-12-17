# --------------------------------------------------------------
#
# Install dependencies for Ubuntu (16).
#
# --------------------------------------------------------------

# Define some paths.
THIS_SCRIPT="${BASH_SOURCE[0]}"
THIS_DIR="$(cd "$(dirname "${THIS_SCRIPT}")" && pwd)"
COMMON_LIB_DIR="$(cd "${THIS_DIR}/../common-lib" && pwd)"

# Include the relevant libraries.
. "${COMMON_LIB_DIR}/utils.bash"
. "${COMMON_LIB_DIR}/slack.bash"

# Which branch of WP should we install?
WP_BRANCH="master"

# Report progress to slack?
REPORT_RESULTS="false"

# Usage message
usage () {
    echo "USAGE: bash $(get_me) [OPTIONS]"
    echo ""
    echo "  Install dependencies for Ubuntu 16."
    echo ""
    echo "OPTIONS"
    echo "  -h | --help       Print this help and exit"
    echo "  --report-results  Report the results to slack"
    echo "  --wp-branch VAL   Install WP from a branch"
}

# Parse the command line arguments.
while (( "${#}" )); do
    case "${1}" in

        -h|--help)
            usage
            exit 1
            ;;

        --report-results)
            REPORT_RESULTS="true"
            ;;

        --wp-branch)
            shift
            WP_BRANCH="${1}"
            ;;

        *)
            echo "Unrecognized argument: ${1}"
            help_hint
            exit 1
            ;;

    esac
    shift
done

# Call `clean_up` before the script exits.
trap clean_up EXIT

# Pass on a --report-results flag?
REPORT_FLAG=""
if [[ "${REPORT_RESULTS}" == "true" ]]; then
    REPORT_FLAG="--report-results"
fi

bash -x "${THIS_DIR}"/install-apt.bash ${REPORT_FLAG}
RESULT="${?}"
if [[ "${RESULT}" != "0" ]]; then
    echo "Failed to install APT packages."
    exit 1
fi

echo ""

bash "${THIS_DIR}"/install-dependencies.bash \
    ${REPORT_FLAG} \
    --wp-branch "${WP_BRANCH}"
RESULT="${?}"
if [[ "${RESULT}" != "0" ]]; then
    echo "Failed to install dependencies."
    exit 1
fi
