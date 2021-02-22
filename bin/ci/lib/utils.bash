# --------------------------------------------------------------
#
# This script defines a number of things (like functions and
# filepaths) that are used by other scripts.
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
#   Returns the filename of this script
get_me () {
    echo "$(basename "${0}")"
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
SLACK_FILE="${TMP_SCRATCH_DIR}/data.json"
BAP_VERSION_FILE="${TMP_SCRATCH_DIR}/bap-version.txt"
GIT_COMMIT_FILE="${TMP_SCRATCH_DIR}/git-commit.txt"
echo "No message yet" > "${MSG_FILE}"
echo "Nothing to report yet" > "${REPORT_FILE}"
echo '{"username":"None yet","text":"Nothing yet"}' > "${SLACK_FILE}"
echo "No BAP version to report yet" > "${BAP_VERSION_FILE}"
echo "No commit to report yet" > "${GIT_COMMIT_FILE}"

# DESC
#   Record the BAP version
bap_version () {
    bap --version > "${BAP_VERSION_FILE}"
    echo "BAP_VERSION: $(cat "${BAP_VERSION_FILE}")"
}

# DESC
#   Record the current GIT commit
git_commit () {
  git log -1 > "${GIT_COMMIT_FILE}"
  echo -e "GIT_COMMIT:\n$(cat "${GIT_COMMIT_FILE}")"
}

# DESC
#   Prints a help hint
help_hint () {
    local ME
    ME="$(get_me)"
    echo "See ${ME} --help for usage." 
}

# DESC
#   Print that we're halting, along with the contents of ${MSG_FILE}.
fail () {
    echo "Halting"
    echo "$(cat "${MSG_FILE}")"
}

# DESC
#   Cleans up files we've written
clean_up () {
    rm -rf "${TMP_SCRATCH_DIR}"
}

# Call `clean_up` before the script exits.
trap clean_up EXIT

# Ensure we're using Bash.
is_bash
if [ ${?} -ne 0 ]; then
    echo "Halting."
    echo "This script must be executed with Bash."
    return 1
fi

# Ensure we can find the root of the repo.
REPO_ROOT="$(cd "${EXEC_DIR}"/../../../ && pwd)"
if [ ! -f "${REPO_ROOT}"/.gitlab-ci.yml ]; then
    echo "Halting."
    echo "Cannot find the repo root."
    echo "Looked in REPO ROOT: '${REPO_ROOT}'"
    echo "But could not find a .gitlab-ci.yml file."
    exit 1
fi

# Note some relevant information.
bap_version
git_branch
git_commit
