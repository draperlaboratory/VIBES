# --------------------------------------------------------------
#
# Install dependencies used by the tools.
#
# --------------------------------------------------------------

# Define some paths.
THIS_SCRIPT="${BASH_SOURCE[0]}"
THIS_DIR="$(cd "$(dirname "${THIS_SCRIPT}")" && pwd)"
COMMON_LIB_DIR="$(cd "${THIS_DIR}/../common-lib" && pwd)"

# Include the relevant libraries.
. "${COMMON_LIB_DIR}/utils.bash"
. "${COMMON_LIB_DIR}/slack.bash"
. "${COMMON_LIB_DIR}/env.bash"

# Report progress to slack?
REPORT_RESULTS="false"

# Usage message
usage () {
    echo "USAGE: bash $(get_me) [OPTIONS]"
    echo ""
    echo "  Install dependencies for the tools."
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

# Install OPAM packages.
opam install -j 3 -y z3 ounit2 ppx_deriving_yojson >> "${REPORT_FILE}" 2>&1
OPAM_RESULT="${?}"
if [[ "${OPAM_RESULT}" != "0" ]]; then
    echo "Unable to install OPAM packages." > "${MSG_FILE}"
    echo "...." >> "${REPORT_FILE}"
    echo "Halting." >> "${REPORT_FILE}"
    echo "Tried to install opam packages." >> "${REPORT_FILE}"
    echo "The attempt returned a non-zero exit code: ${OPAM_RESULT}." >> "${REPORT_FILE}"
    echo "$(cat "${MSG_FILE}")"
    echo "$(cat "${REPORT_FILE}")"
    if [[ "${REPORT_RESULTS}" == "true" ]]; then
        report_to_slack
    fi
    exit 1
else
    echo "- OPAM packages installed." | tee -a "${REPORT_FILE}"
fi

# If WP is not installed, install it.
WP_IS_INSTALLED="$(bap list plugins | grep wp | grep 'weakest precondition')"
if [ -z "${WP_IS_INSTALLED}" ]; then
    CURRENT_DIR="$(pwd)"
    CBAT_DIR="${HOME}/cbat_tools"
    cd "${HOME}"
    if [ -d "${CBAT_DIR}" ]; then
        echo "Can't clone 'cbat_tools'" > "${MSG_FILE}"
        echo "Halting." >> "${REPORT_FILE}"
        echo "Want to clone 'cbat_tools', but can't." >> "${REPORT_FILE}"
        echo "${CBAT_DIR} already exists." >> "${REPORT_FILE}"
        echo "$(cat "${MSG_FILE}")"
        echo "$(cat "${REPORT_FILE}")"
	if [[ "${REPORT_RESULTS}" == "true" ]]; then
            report_to_slack
        fi
        exit 1
    fi
    git clone https://github.com/draperlaboratory/cbat_tools.git
    cd cbat_tools/wp
    make
    cd "${CURRENT_DIR}"
fi

# Make sure WP got installed.
WP_IS_INSTALLED="$(bap list plugins | grep wp | grep 'weakest precondition')"
if [ -z "${WP_IS_INSTALLED}" ]; then
    echo "Unable to find 'wp'" > "${MSG_FILE}"
    echo "Halting." >> "${REPORT_FILE}"
    echo "WP does not seem to be installed." >> "${REPORT_FILE}"
    echo "Grepped 'bap list plugins' for 'wp'." >> "${REPORT_FILE}"
    echo "Only got this: '${WP_IS_INSTALLED}'" >> "${REPORT_FILE}"
    echo "$(cat "${MSG_FILE}")"
    echo "$(cat "${REPORT_FILE}")"
    if [[ "${REPORT_RESULTS}" == "true" ]]; then
        report_to_slack
    fi
    exit 1
else
    echo "- WP is installed." | tee -a "${REPORT_FILE}"
fi

# If minizinc isn't already installed, install it.
which minizinc > /dev/null 2>&1
if [[ "${?}" != "0" ]]; then
    CURRENT_DIR="$(pwd)"
    cd "${HOME}"
    if [ -d "${MINIZINC_DIR}" ]; then
        echo "Can't untar minizinc package" > "${MSG_FILE}"
        echo "Halting." >> "${REPORT_FILE}"
        echo "Want to download/untar minizinc, but can't." >> "${REPORT_FILE}"
        echo "${MINIZINC_DIR} already exists." >> "${REPORT_FILE}"
        echo "$(cat "${MSG_FILE}")"
        echo "$(cat "${REPORT_FILE}")"
        if [[ "${REPORT_RESULTS}" == "true" ]]; then
            report_to_slack
        fi
        exit 1
    fi
    curl -L "${MINIZINC_URL}" --output minizinc.tgz
    tar zxvf minizinc.tgz
    cd "${CURRENT_DIR}"
fi

# Make sure minizinc got installed.
which minizinc > /dev/null 2>&1
if [[ "${?}" != "0" ]]; then
    echo "Unable to find minizinc" > "${MSG_FILE}"
    echo "Halting." > "${REPORT_FILE}"
    echo "Minizinc does not seem to be installed.." >> "${REPORT_FILE}"
    echo "Tried 'which minizinc' but got nothing." >> "${REPORT_FILE}"
    echo "$(cat "${MSG_FILE}")"
    echo "$(cat "${REPORT_FILE}")"
    if [[ "${REPORT_RESULTS}" == "true" ]]; then
        report_to_slack
    fi
    exit 1
else
    echo "- Minizinc is installed." | tee -a "${REPORT_FILE}"
fi

# If boolector isn't already installed, install it.
which boolector > /dev/null 2>&1
if [[ "${?}" != "0" ]]; then
    CURRENT_DIR="$(pwd)"
    cd "${HOME}"
    if [ -d "${BOOLECTOR_DIR}" ]; then
        echo "Can't git pull the boolector repo" > "${MSG_FILE}"
	echo "Halting." >> "${REPORT_FILE}"
	echo "Want to pull the boolector repo, but can't." >> "${REPORT_FILE}"
	echo "${BOOLECTOR_DIR} already exists." >> "${REPORT_FILE}"
	echo "$(cat "${MSG_FILE}")"
	echo "$(cat "${REPORT_FILE}")"
	if [[ "${REPORT_RESULTS}" == "true" ]]; then
            report_to_slack
        fi
	exit 1
    fi
    git clone "${BOOLECTOR_URL}"
    cd boolector
    ./contrib/setup-lingeling.sh
    ./contrib/setup-btor2tools.sh
    ./configure.sh && cd build && make
    cd "${CURRENT_DIR}"
fi

# Make sure boolector got installed.
which boolector > /dev/null 2>&1
if [[ "${?}" != "0" ]]; then
    echo "Unable to find boolector" > "${MSG_FILE}"
    echo "Halting." > "${REPORT_FILE}"
    echo "Boolector does not seem to be installed.." >> "${REPORT_FILE}"
    echo "Tried 'which boolector' but got nothing." >> "${REPORT_FILE}"
    echo "$(cat "${MSG_FILE}")"
    echo "$(cat "${REPORT_FILE}")"
    if [[ "${REPORT_RESULTS}" == "true" ]]; then
        report_to_slack
    fi
    exit 1
else
    echo "- Boolector is installed." | tee -a "${REPORT_FILE}"
fi

# Finish up.
echo "Done." | tee -a "${REPORT_FILE}"
if [[ "${REPORT_RESULTS}" == "true" ]]; then
    echo "Installed dependencies" > "${MSG_FILE}"
    report_to_slack
fi
