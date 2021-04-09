# --------------------------------------------------------------
#
# This script has utilities for sending messages to slack.
#
# It relies on the following being sourced already:
#   - utils.bash - which sets up some tmp directories and files
#                  used by the functions here
#
# Note that these environment variables must be set:
#   - SLACK_USERNAME - A username to post to slack with
#   - SLACK_URL - The URL to post the message to
#
# --------------------------------------------------------------

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
    if [ -z "${SLACK_URL}" ];
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
    local BRANCH
    local COMMIT
    local DATA
    local TEXT
    MESSAGE="$(cat "${MSG_FILE}")"
    BAP="$(cat "${BAP_VERSION_FILE}")"
    COMMIT="$(sed -z -e 's/\n/\\n/g' -e 's/\"/\\"/g' "${GIT_COMMIT_FILE}")"
    DATA="$(sed -z -e 's/\n/\\n/g' -e 's/\"/\\"/g' "${REPORT_FILE}")"
    TEXT="STATUS: ${MESSAGE}"
    TEXT="${TEXT}\nBAP: ${BAP}"
    TEXT="${TEXT}\nCOMMIT:\n\`\`\`\n${COMMIT}\n\`\`\`"
    TEXT="${TEXT}\nOUTPUT:\n\`\`\`\n${DATA}\n\`\`\`"
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
    local OUTPUT
    local RESULT
    OUTPUT="$(curl \
        -X POST \
        -H "Content-Type: application/json" \
        -d @"${SLACK_FILE}" \
	"${SLACK_URL}")"
    RESULT=${?}
    echo "${OUTPUT}"
    return ${RESULT}
}

# DESC
#   Report the current status of things to slack.
# RETURNS
#   The exit code of the attempt to send the message to slack.
report_to_slack () {
    build_slack_payload
    post_to_slack
}