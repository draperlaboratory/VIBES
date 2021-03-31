# --------------------------------------------------------------
#
# This script provides utilities for running system tests.
#
# Note: This relies on utils.bash being sourced already.
#
# --------------------------------------------------------------

# Define some paths.
THIS_SCRIPT="${BASH_SOURCE[0]}"
LIB_DIR="$(cd "$(dirname "${THIS_SCRIPT}")" && pwd)"

# Include the relevant libraries.
. "${LIB_DIR}/pretty.bash"

# For tallying up results.
TESTS_FINAL_STATUS="PASSED"
TESTS_TALLY=""
TESTS_PASSED=0
TESTS_FAILED=0

# For recording progress.
if [[ -z "${REPORT}" ]]; then
    REPORT=/dev/null
fi

# DESC
#   Print startup info
# ARGS
# - 1 : A file to record results in
print_test_startup_info () {
    rule | tee "${REPORT}"
    bold "|| RUNNING SYSTEM TESTS..." | tee -a "${REPORT}"
    echo "" | tee -a "${REPORT}"
    echo "|| Recording progress to: ${REPORT}" | tee -a "${REPORT}"
    rule_light | tee -a "${REPORT}"
}

# DESC
#   Print a header
# ARGS
# - 1 : The message to print
print_header () {
    echo "" | tee -a "${REPORT}"
    rule | tee -a "${REPORT}"
    bold "|| ${1}" | tee -a "${REPORT}"
    echo "" | tee -a "${REPORT}"
    rule | tee -a "${REPORT}"
    echo "" | tee -a "${REPORT}"
}

# DESC
#   Print summary info
print_summary () {
    echo "" | tee -a "${REPORT}"
    rule_light | tee -a "${REPORT}"
    echo -n "|| Finished system tests: " | tee -a "${REPORT}"
    if [[ "${TESTS_FINAL_STATUS}" == "PASSED" ]]; then
        green "${TESTS_FINAL_STATUS}" | tee -a "${REPORT}"
    else
        red "${TESTS_FINAL_STATUS}" | tee -a "${REPORT}"
    fi
    echo "" | tee -a "${REPORT}"
    echo "|| ${TESTS_TALLY}" | tee -a "${REPORT}" 
    echo "|| Passed: ${TESTS_PASSED}," \
	 "Failed: ${TESTS_FAILED}" | tee -a "${REPORT}"
    rule | tee -a "${REPORT}"
}

# DESC
#     Run an ARM executable with qemu
# ARGS
# - 1 : The path to the executable
# - 2 : The expected exit code
run_arm_exe () {
    local EXE_PATH="${1}"
    local EXPECTED_EXIT_CODE="${2}"
    local LD_PREFIX="/usr/arm-linux-gnueabi"
    local ACTUAL_EXIT_CODE
    local OUTPUT
    echo "- Running ${EXE_PATH}" | tee -a "${REPORT}"
    OUTPUT="$(QEMU_LD_PREFIX=${LD_PREFIX} qemu-arm "${EXE_PATH}" 2>&1)"
    ACTUAL_EXIT_CODE="${?}"
    if [[ "${ACTUAL_EXIT_CODE}" != "${EXPECTED_EXIT_CODE}" ]]; then
        red "  - Error. Expected exit code ${EXPECTED_EXIT_CODE}, "
        red "but got ${ACTUAL_EXIT_CODE}"
        echo ""
        echo -n "  - Error. " >> "${REPORT}"
	echo -n "Expected exit code ${EXPECTED_EXIT_CODE}, " >> "${REPORT}"
	echo "but got ${ACTUAL_EXIT_CODE}" >> "${REPORT}" 
	if [[ -z "${OUTPUT}" ]]; then
	    echo "  - Program output:" | tee -a "${REPORT}"
	    echo "    The program printed no output." |tee -a "${REPORT}"
	else
	    echo "  - Program output:" | tee -a "${REPORT}"
	    rule_light | tee -a "${REPORT}"
	    echo "${OUTPUT}" | tee -a "${REPORT}"
	    rule_light | tee -a "${REPORT}"
	fi
        echo ""
        TESTS_FINAL_STATUS="FAILED"
        TESTS_TALLY="${TESTS_TALLY}F"
        let TESTS_FAILED++
    else
        green "  - Ok. Expected exit code ${EXPECTED_EXIT_CODE}, "
        green "and got ${ACTUAL_EXIT_CODE}"
        echo ""
	echo -n "  - Ok. " >> "${REPORT}"
	echo -n "Expected exit code ${EXPECTED_EXIT_CODE}, " >> "${REPORT}"
	echo "and got ${ACTUAL_EXIT_CODE}" >> "${REPORT}"
        echo ""
        TESTS_TALLY="${TESTS_TALLY}."
        let TESTS_PASSED++
    fi
}

# DESC
#     Run a make command
# ARGS
# - 1 : The make command
# - 2 : The expected exit code
run_make () {
    local CMD="${1}"
    local EXPECTED_EXIT_CODE="${2}"
    local ACTUAL_EXIT_CODE
    local OUTPUT
    echo "- Running '${CMD}'..." | tee -a "${REPORT}"
    OUTPUT="$(${CMD} 2>&1)"
    ACTUAL_EXIT_CODE="${?}"
    if [[ "${ACTUAL_EXIT_CODE}" != "${EXPECTED_EXIT_CODE}" ]]; then
        red "  - Error. Expected exit code ${EXPECTED_EXIT_CODE}, "
        red "but got ${ACTUAL_EXIT_CODE}"
        echo ""
        echo -n "  - Error. " >> "${REPORT}"
	echo -n "Expected exit code ${EXPECTED_EXIT_CODE}, " >> "${REPORT}"
	echo "but got ${ACTUAL_EXIT_CODE}" >> "${REPORT}" 
	if [[ -z "${OUTPUT}" ]]; then
	    echo "  - Program output:" | tee -a "${REPORT}"
	    echo "    The program printed no output." | tee -a "${REPORT}"
	else
	    echo "  - Program output:" | tee -a "${REPORT}"
	    rule_light | tee -a "${REPORT}"
	    echo "${OUTPUT}" | tee -a "${REPORT}"
	    rule_light | tee -a "${REPORT}"
	fi
        echo ""
        TESTS_FINAL_STATUS="FAILED"
        TESTS_TALLY="${TESTS_TALLY}F"
        let TESTS_FAILED++
    else
        green "  - Ok. Expected exit code ${EXPECTED_EXIT_CODE}, "
        green "and got ${ACTUAL_EXIT_CODE}"
        echo ""
	echo -n "  - Ok. " >> "${REPORT}"
	echo -n "Expected exit code ${EXPECTED_EXIT_CODE}, " >> "${REPORT}"
	echo "and got ${ACTUAL_EXIT_CODE}" >> "${REPORT}"
        echo ""
        TESTS_TALLY="${TESTS_TALLY}."
        let TESTS_PASSED++
    fi
}
