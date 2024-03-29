# --------------------------------------------------------------
#
# This script provides some basic utilities
# (used when running system tests).
#
# --------------------------------------------------------------

# DESC
#   Check if this script is running with bash
# RETURNS
#   - 0 if this script is running with bash
#   - 1 if not
is_bash () {
    if [ -z "${BASH_VERSION}" ];
    then return 1;
    else return 0;
    fi
}

# DESC
#   Redirects an echo to stderr
report() {
    echo "$@" 1>&2
}

# DESC
#   Returns the filename of this script
get_me () {
    echo "$(basename "${0}")"
}

# DESC
#   Prints a help hint
help_hint () {
    local ME
    ME="$(get_me)"
    echo "See ${ME} --help for usage." 
}

# DESC
#   Cleans up scratch files etc.
clean_up () {
  # Doing nothing at the moment
  return 0
}


# DESC
#   Constructs a tmp dir for use by this script
# RETURNS
#   The tmp dir name
create_tmp_dir () {
    local DIR_NAME
    local ME
    ME="$(get_me)"
    DIR_NAME="$(mktemp -d "${TMPDIR:-/tmp/}${ME}.XXXXXXXXXXXX")"
    echo "${DIR_NAME}"
}

# Setup tmp dir/files
TMP_SCRATCH_DIR="$(create_tmp_dir)"
MSG_FILE="${TMP_SCRATCH_DIR}/message.txt"
REPORT_FILE="${TMP_SCRATCH_DIR}/report.txt"
SUMMARY_FILE="${TMP_SCRATCH_DIR}/summary.txt"
SLACK_FILE="${TMP_SCRATCH_DIR}/data.json"
BAP_VERSION_FILE="${TMP_SCRATCH_DIR}/bap-version.txt"
GIT_BRANCH_FILE="${TMP_SCRATCH_DIR}/git-branch.txt"
GIT_COMMIT_FILE="${TMP_SCRATCH_DIR}/git-commit.txt"
echo "Initializing message...no message yet" > "${MSG_FILE}"
echo "Initializing report...nothing to report yet" > "${REPORT_FILE}"
echo "Initializing summary...nothing to report yet" > "${SUMMARY_FILE}"
echo '{"username":"None yet","text":"Nothing yet"}' > "${SLACK_FILE}"
echo "No BAP version to report yet" > "${BAP_VERSION_FILE}"
echo "No git branch to report yet" > "${GIT_BRANCH_FILE}"
echo "No git commit to report yet" > "${GIT_COMMIT_FILE}"

# DESC
#     Get filepath to the SUMMARY_FILE.
# ARGS
# - ${1} : If "true" then return SUMMARY_FILE.
#          Otherwise, return /dev/null.
summary_file () {
    if [[ "${1}" == "true" ]]; then
        echo "${SUMMARY_FILE}"
    else
        echo "/dev/null"
    fi
}

# DESC
#     Get filepath to the REPORT_FILE.
# ARGS
# - ${1} : If "true" then return REPORT_FILE.
#          Otherwise, return /dev/null.
report_file () {
    if [[ "${1}" == "true" ]]; then
        echo "${REPORT_FILE}"
    else
        echo "/dev/null"
    fi
}

# DESC
#   Record the BAP version
bap_version () {
    bap --version > "${BAP_VERSION_FILE}"
    echo "BAP_VERSION: $(cat "${BAP_VERSION_FILE}")"
}

# DESC
#   Record the current GIT branch
git_branch () {
    git branch | grep "\*" --color=never > "${GIT_BRANCH_FILE}"
    echo "GIT_BRANCH: $(cat "${GIT_BRANCH_FILE}")"
}

# DESC
#   Record the current GIT commit
git_commit () {
    git log -1 > "${GIT_COMMIT_FILE}"
    if [[ -z "$(cat "${GIT_COMMIT_FILE}")" ]]; then
        echo "${CI_COMMIT_MESSAGE}" > "${GIT_COMMIT_FILE}"
        if [[ -z "$(cat "${GIT_COMMIT_FILE}")" ]]; then
            echo "No commit message could be obtained" > "${GIT_COMMIT_FILE}"
        fi
    fi
    echo -e "GIT_COMMIT:\n$(cat "${GIT_COMMIT_FILE}")"
}

# DESC
#   Cleans up files we've written
clean_up () {
    rm -rf "${TMP_SCRATCH_DIR}"
}

# DESC
#   Print that we're halting, along with the contents of ${MSG_FILE}.
fail () {
    echo "Halting"
    echo "$(cat "${MSG_FILE}")"
}

# Ensure we're using Bash.
is_bash
if [ ${?} -ne 0 ]; then
    echo "Halting."
    echo "This script must be executed with Bash."
    return 1
fi

# Where are we?
THIS_SCRIPT="${BASH_SOURCE[0]}"
COMMON_LIB_DIR="$(cd "$(dirname "${THIS_SCRIPT}")" && pwd)"

# Ensure we can find the root of the repo.
REPO_ROOT="$(cd "${COMMON_LIB_DIR}"/../../ && pwd)"
if [ ! -f "${REPO_ROOT}"/README.md ]; then
    echo "Halting."
    echo "Cannot find the repo root."
    echo "Looked in REPO ROOT: '${REPO_ROOT}'"
    echo "But could not find a README.md file."
    exit 1
fi

# Where these scripts are all kept.
BIN_DIR="${REPO_ROOT}/bin"

# Where the executables are kept.
EXES_DIR="$(cd "${REPO_ROOT}/resources/exes" && pwd)"
