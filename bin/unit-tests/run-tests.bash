# --------------------------------------------------------------
#
# Run the unit tests.
#
# --------------------------------------------------------------

# Define some paths.
THIS_SCRIPT="${BASH_SOURCE[0]}"
SETUP_DIR="$(cd "$(dirname "${THIS_SCRIPT}")" && pwd)"
COMMON_LIB_DIR="$(cd "${SETUP_DIR}/../common-lib" && pwd)"

# Include the relevant libraries.
. "${COMMON_LIB_DIR}/utils.bash"
. "${COMMON_LIB_DIR}/slack.bash"

# Report progress to slack?
REPORT_RESULTS="false"

# Usage message
usage () {
    echo "USAGE: bash $(get_me) [OPTIONS]"
    echo ""
    echo "  Run the unit tests."
    echo ""
    echo "OPTIONS"
    echo "  -h | --help       Print this help and exit"
    echo "  --report-results  Report the results to slack"
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

# Ensure we have a slack username and URL to post with.
if [[ "${REPORT_RESULTS}" == "true" ]]; then
    there_is_a_SLACK_USERNAME
    if [ ${?} -ne 0 ]; then
        echo "Halting."
        echo "Need a SLACK_USERNAME environment variable."
        echo "Export one to proceed."
        exit 1
    fi
    there_is_a_SLACK_URL
    if [ ${?} -ne 0 ]; then
        echo "Halting."
        echo "Need a SLACK_URL environment variable."
        echo "Export one to proceed."
        exit 1
    fi
fi

# Where to record progress.
REPORT="$(report_file "${REPORT_RESULTS}")"

# Record some useful info.
bap_version
git_commit

echo ""

# Prep for test runs.
make clean -C "${REPO_ROOT}"/bap-vibes > "${REPORT_FILE}" 2>&1

# Run the unit tests.
make test.unit -C "${REPO_ROOT}" >> "${REPORT_FILE}" 2>&1
TEST_RESULT="${?}"
echo "REPORT:"
cat "${REPORT_FILE}"
if [[ "${TEST_RESULT}" != "0" ]]; then
    echo "Unit tests failed" | tee "${MSG_FILE}"
    if [[ "${REPORT_RESULTS}" == "true" ]]; then
        report_to_slack
    fi
    fail
    exit 1
else
    echo "Unit tests passed" | tee "${MSG_FILE}"
    if [[ "${REPORT_RESULTS}" == "true" ]]; then
        report_to_slack
    fi
fi
