# --------------------------------------------------------------
#
# Run one system test.
#
# --------------------------------------------------------------

# Define some paths.
THIS_SCRIPT="${BASH_SOURCE[0]}"
THIS_DIR="$(cd "$(dirname "${THIS_SCRIPT}")" && pwd)"
COMMON_LIB_DIR="$(cd "${THIS_DIR}/../common-lib" && pwd)"
TESTS_DIR="$(cd "${THIS_DIR}/tests" && pwd)"

# Include the relevant libraries.
. "${COMMON_LIB_DIR}/utils.bash"
. "${COMMON_LIB_DIR}/slack.bash"
. "${COMMON_LIB_DIR}/system-testing.bash"

# Report progress to slack?
REPORT_RESULTS="false"
SUMMARY_RESULTS="false"

# Report more than just errors?
REPORT_VERBOSITY="quiet"

# The name of the test to run.
TEST_NAME=""

# Usage message
usage () {
    echo "USAGE: bash $(get_me) [OPTIONS] TEST_NAME"
    echo ""
    echo "  Run a system test, where TEST_NAME is the name of"
    echo "  one of the functions defined in this directory:"
    echo "  ${TESTS_DIR}"
    echo ""
    echo "OPTIONS"
    echo "  -h | --help       Print this help and exit"
    echo "  --report-results  Report the results to slack"
    echo "  --not-quiet       Report full results, not just a summary"
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
            SUMMARY_RESULTS="true"
            ;;

        --not-quite)
            REPORT_VERBOSITY="verbose"
            ;;

        *)
            if [[ -z "${TEST_NAME}" ]]; then
                TEST_NAME="${1}"
            else
                echo "Unrecognized argument: ${1}"
                help_hint
                exit 1
            fi
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
SUMMARY="$(summary_file "${SUMMARY_RESULTS}")"

# Record some useful info.
bap_version
git_branch
git_commit

# Note that we're starting the tests.
print_test_startup_info

# Import the tests.
for FILEPATH in "${TESTS_DIR}"/test-*.bash; do
    . ${FILEPATH}
done

# Make sure the test exists.
if [[ $(type -t "${TEST_NAME}") != function ]]; then
    echo ""
    echo "No such test: ${TEST_NAME}"
    exit 1
fi

# Run the test.
"${TEST_NAME}"

# Final report for the test results.
print_summary

# Report the results (if needed) and exit.
if [[ "${TESTS_FINAL_STATUS}" == "FAILED" ]]; then
    if [[ "${REPORT_RESULTS}" == "true" ]]; then
        echo "Posting results to slack..."
        echo "System tests failed" > "${MSG_FILE}"
        report_to_slack "${REPORT_VERBOSITY}"
    fi
    exit 1
else
    if [[ "${REPORT_RESULTS}" == "true" ]]; then
        echo "Posting results to slack..."
        echo "System tests passed" > "${MSG_FILE}"
        report_to_slack "${REPORT_VERBOSITY}"
    fi
    exit 0
fi
