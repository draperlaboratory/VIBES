# --------------------------------------------------------------
#
# Run the system tests.
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

# Usage message
usage () {
    echo "USAGE: bash $(get_me) [OPTIONS]"
    echo ""
    echo "  Run the system tests."
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
	    SUMMARY_RESULTS="true"
            ;;

        --not-quiet)
            REPORT_VERBOSITY="verbose"
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
SUMMARY="$(summary_file "${SUMMARY_RESULTS}")"

# Record some useful info.
bap_version
git_branch
git_commit

# Note that we're starting the tests.
print_test_startup_info

# Run every test.
for FILEPATH in "${TESTS_DIR}"/test-*.bash; do
    . ${FILEPATH}
done

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
