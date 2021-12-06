# --------------------------------------------------------------
#
# Install APT packages assumed by the tools.
#
# --------------------------------------------------------------

# Define some paths.
THIS_SCRIPT="${BASH_SOURCE[0]}"
THIS_DIR="$(cd "$(dirname "${THIS_SCRIPT}")" && pwd)"
COMMON_LIB_DIR="$(cd "${THIS_DIR}/../common-lib" && pwd)"

# Include the relevant libraries.
. "${COMMON_LIB_DIR}/utils.bash"
. "${COMMON_LIB_DIR}/slack.bash"
. "${THIS_DIR}/env.bash"

# Report progress to slack?
REPORT_RESULTS="false"

# Usage message
usage () {
    echo "USAGE: bash $(get_me) [OPTIONS]"
    echo ""
    echo "  Install APT packages assumed by tools."
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
git_branch
git_commit

echo ""

# Update APT repository information
sudo apt update
APT_UPDATE_RESULT="${?}"
if [[ "${APT_UPDATE_RESULT}" != "0" ]]; then
    echo "Unable to update APT repository information." > "${MSG_FILE}"
    echo "...." >> "${REPORT_FILE}"
    echo "Halting." >> "${REPORT_FILE}"
    echo "Tried 'sudo apt update'." >> "${REPORT_FILE}"
    echo "Got a non-zero exit code: ${APT_RESULT}." >> "${REPORT_FILE}"
    echo "$(cat "${MSG_FILE}")"
    echo "$(cat "${REPORT_FILE}")"
    if [[ "${REPORT_RESULTS}" == "true" ]]; then
        report_to_slack
    fi
    exit 1
else
    echo "- APT repository information updated." | tee -a "${REPORT_FILE}"
fi

# Install APT dependencies.
sudo apt install -y \
    qemu \
    binutils-arm-linux-gnueabi \
    gcc-arm-linux-gnueabi \
    cmake
APT_RESULT="${?}"
if [[ "${APT_RESULT}" != "0" ]]; then
    echo "Unable to install APT packages." > "${MSG_FILE}"
    echo "...." >> "${REPORT_FILE}"
    echo "Halting." >> "${REPORT_FILE}"
    echo "Tried to install APT packages." >> "${REPORT_FILE}"
    echo "Got a non-zero exit code: ${APT_RESULT}." >> "${REPORT_FILE}"
    echo "$(cat "${MSG_FILE}")"
    echo "$(cat "${REPORT_FILE}")"
    if [[ "${REPORT_RESULTS}" == "true" ]]; then
        report_to_slack
    fi
    exit 1
else
    echo "- APT packages installed." | tee -a "${REPORT_FILE}"
fi

# Finish up.
echo "Done." | tee -a "${REPORT_FILE}"
if [[ "${REPORT_RESULTS}" == "true" ]]; then
    echo "Installed APT packages" > "${MSG_FILE}"
    report_to_slack
fi
