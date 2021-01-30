# --------------------------------------------------------------
#
# This script installs the dependencies.
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

# If WP is not installed, install it.
WP_IS_INSTALLED="$(bap list plugins | grep wp | grep 'weakest precondition')"
if [ -z "${WP_IS_INSTALLED}" ]; then
    CURRENT_DIR="$(pwd)"
    CBAT_DIR="${HOME}/cbat_tools"
    cd "${HOME}"
    if [ -d "${CBAT_DIR}" ]; then
	echo "Can't clone 'cbat_tools'" > "${MSG_FILE}"
        echo "Halting." > "${REPORT_FILE}"
	echo "Want to clone 'cbat_tools', but can't." >> "${REPORT_FILE}"
        echo "${CBAT_DIR} already exists." >> "${REPORT_FILE}"
	echo "$(cat "${MSG_FILE}")"
	echo "$(cat "${REPORT_FILE}")"
	report_to_slack
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
    echo "Halting." > "${REPORT_FILE}"
    echo "WP does not seem to be installed." >> "${REPORT_FILE}"
    echo "Grepped 'bap list plugins' for 'wp'." >> "${REPORT_FILE}"
    echo "Only got this: '${WP_IS_INSTALLED}'" >> "${REPORT_FILE}"
    echo "$(cat "${MSG_FILE}")"
    echo "$(cat "${REPORT_FILE}")"
    report_to_slack
    exit 1
else
    echo "WP is installed."
fi

# If minizinc isn't already installed, install it.
which minizinc > /dev/null 2>&1
if [[ "${?}" != "0" ]]; then
    CURRENT_DIR="$(pwd)"
    cd "${HOME}"
    if [ -d "${MINIZINC_DIR}" ]; then
        echo "Can't untar minizinc package" > "${MSG_FILE}"
        echo "Halting." > "${REPORT_FILE}"
	echo "Want to download/untar minizinc, but can't." >> "${REPORT_FILE}"
        echo "${MINIZINC_DIR} already exists." >> "${REPORT_FILE}"
        echo "$(cat "${MSG_FILE}")"
        echo "$(cat "${REPORT_FILE}")"
        report_to_slack
        exit 1
    fi
    curl -L "${MINIZINC_URL}" --output minizinc.tgz
    tar zxvf minizinc.tgz
    cd "${CURRET_DIR}"
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
    report_to_slack
    exit 1
else
    echo "Minizinc is installed."
fi

# Install OPAM packages.
opam install -y ounit2 ppx_deriving_yojson > "${REPORT_FILE}" 2>&1
if [[ "${?}" != "0" ]]; then
    echo "Unable to install OPAM packages." > "${MSG_FILE}"
    echo "...." >> "${REPORT_FILE}"
    echo "Halting." >> "${REPORT_FILE}"
    echo "Tried to install opam packages." >> "${REPORT_FILE}"
    echo "The attempt returned a non-zero exit code." >> "${REPORT_FILE}"
    echo "$(cat "${MSG_FILE}")"
    echo "$(cat "${REPORT_FILE}")"
    report_to_slack
    exit 1
else
    echo "OPAM packages installed."
fi
