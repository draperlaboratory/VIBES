# --------------------------------------------------------------
#
# This script does the following:
#   Installs minizinc in the test runner
#
# To run it:
#   bash -x install-minizinc.bash
#
# --------------------------------------------------------------

# DESC
#   Returns the filename of this script
get_me () {
    echo "$(basename "${0}")"
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
#   Returns a help/usage message
usage () {
    local ME
    ME="$(get_me)"
    echo "USAGE: ${ME} [OPTIONS]"
    echo ""
    echo "  Install minizinc in the test runner"
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
