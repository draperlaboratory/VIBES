# --------------------------------------------------------------
#
# This script is a basic hello-world test.
# Run it to make sure things are working.
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

# Construct a simple report, echo it here, and send it to Slack.
echo "Hello world!" > "${MSG_FILE}"
echo "Reporting 'hello world'." > "${REPORT_FILE}"
echo "$(cat "${MSG_FILE}")"
echo "$(cat "${REPORT_FILE}")"
report_to_slack
