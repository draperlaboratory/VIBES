# --------------------------------------------------------------
#
# This script runs the full test suite (unit and integration).
#
# --------------------------------------------------------------

# Define some paths.
THIS_SCRIPT="${BASH_SOURCE[0]}"
EXEC_DIR="$(cd "$(dirname "${THIS_SCRIPT}")" && pwd)"
LIB_DIR="$(cd "${EXEC_DIR}/../lib" && pwd)"

# Include the relevant libraries.
. "${LIB_DIR}/utils.bash"
. "${LIB_DIR}/slack.bash"
. "${LIB_DIR}/env.bash"

# Parse the command line arguments.
while (( "${#}" )); do
    case "${1}" in

        -h|--help)
            usage
            exit 1
            ;;

        *)
            echo "Unrecognized argument: ${1}"
            help_hint
            exit 1
            ;;

    esac
    shift
done

# Run the tests.
make clean -C "${REPO_ROOT}"/bap-vibes > "${REPORT_FILE}" 2>&1
make test -C "${REPO_ROOT}" >> "${REPORT_FILE}" 2>&1
echo "REPORT:"
cat "${REPORT_FILE}"
if [ ${?} -ne 0 ]; then
    echo "Tests failed" > "${MSG_FILE}"
    report_to_slack
    fail
    exit 1
else
    echo "Tests passed" > "${MSG_FILE}"
    report_to_slack
fi
