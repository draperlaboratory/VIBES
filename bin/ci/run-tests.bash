# --------------------------------------------------------------
#
# This script does the following:
#   Runs tests in the local repo and posts the result to slack
#
# To run it:
#   bash -x run-tests.bash
#
# These environment variables must be set:
#   - SLACK_USERNAME
#   - SLACK_URL
#
# --------------------------------------------------------------

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
echo "No message yet" > "${MSG_FILE}"
echo "Nothing to report yet" > "${REPORT_FILE}"
echo '{"username":"run-test.sh","text":"Nothing yet"}' > "${SLACK_FILE}"
echo "No BAP version to report yet" > "${BAP_VERSION_FILE}"

# DESC
#   Record the BAP version
bap_version () {
    bap --version > "${BAP_VERSION_FILE}"
}

# DESC
#   Check if a SLACK_USERNAME environment variable is defined/non-empty.
# RETURNS
#   - 0 if it is defined/non-empty
#   - 1 otherwise
there_is_a_SLACK_USERNAME () {
    if [ -z "${SLACK_USERNAME+x}" ];
    then return 1;
    else return 0;
    fi
}

# DESC
#   Check if a SLACK_URL environment variable is defined/non-empty.
# RETURNS
#   - 0 if it is defined/non-empty
#   - 1 otherwise
there_is_a_SLACK_URL () {
    if [ -z "${SLACK_URL+x}" ];
    then return 1;
    else return 0;
    fi
}

# DESC
#   Check if this script is running with bash
# RETURNS
#   - 0 if this script is running with bash
#   - 1 if not
is_bash () {
    if [ -z "${BASH_VERSION+x}" ];
    then return 1;
    else return 0;
    fi
}

# DESC
#   Build a slack payload (a JSON file) to send.
#   The contents are constructed from ${MSG_FILE} and ${REPORT_FILE}.
# RETURNS
#   The exit code of the attempt to write the file.
build_slack_payload () {
    local MESSAGE
    local BAP
    local DATA
    local TEXT
    MESSAGE="$(cat "${MSG_FILE}")"
    BAP="$(cat "${BAP_VERSION_FILE}")"
    DATA="$(sed -z -e 's/\n/\\n/g' -e 's/\"/\\"/g' "${REPORT_FILE}")"
    TEXT="STATUS: ${MESSAGE}\nBAP: ${BAP}\n\`\`\`\nOUTPUT:\n${DATA}\n\`\`\`"
    echo "{
        \"username\":\"${SLACK_USERNAME}\",
        \"text\":\"${TEXT}\"
    }" > "${SLACK_FILE}"
}

# DESC
#   Post a message to slack
# RETURNS
#   The exit code of the curl/POST command
post_to_slack () {
    curl \
        -X POST \
        -H "Content-Type: application/json" \
        -d @"${SLACK_FILE}" \
        "${SLACK_URL}"
}

# DESC
#   Report the current status of things to slack.
# RETURNS
#   The exit code of the attempt to send the message to slack.
report_to_slack () {
    build_slack_payload
    post_to_slack
}

# DESC
#   Returns a help/usage message
usage () {
    local ME
    ME="$(get_me)"
    echo "USAGE: ${ME} [OPTIONS]"
    echo ""
    echo "  Run tests in the local repo and post the result to slack"
    echo ""
    echo "OPTIONS:"
    echo "  -h/--help   Display this help"
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

# DESC
#   Run the tests and stash the output in ${REPORT_FILE}
# RETURNS
#   The exit code of the attempt to run the tests.
run_tests () {
    make test > "${REPORT_FILE}" 2>&1
}


# --------------------------------------------------------------
# Parse command line arguments

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

trap clean_up INT TERM


# --------------------------------------------------------------
# The main program

# Ensure we're using Bash.
is_bash
if [ ${?} -ne 0 ]; then
    echo "Halting."
    echo "This script must be executed with Bash."
    exit 1
fi

# Ensure we have a slack username to post with.
there_is_a_SLACK_USERNAME
if [ ${?} -ne 0 ]; then
    echo "Halting."
    echo "No SLACK_USERNAME environment variable is defined."
    exit 1
fi

# Ensure we have a slack URL to post to.
there_is_a_SLACK_URL
if [ ${?} -ne 0 ]; then
    echo "Halting."
    echo "No SLACK_URL environment variable is defined."
    exit 1
fi

# Record the BAP version.
bap_version
cat "${BAP_VERSION_FILE}"

# Run the tests.
run_tests
if [ ${?} -ne 0 ]; then
    echo "Tests failed" > "${MSG_FILE}"
    report_to_slack
    fail
    exit 1
else
    echo "Tests passed" > "${MSG_FILE}"
    report_to_slack
fi

# All finished.
clean_up
