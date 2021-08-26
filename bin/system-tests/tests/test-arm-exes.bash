# --------------------------------------------------------------
#
# This script tests the "arm-*" exes.
#
# --------------------------------------------------------------

test_arm_simple () {
    local TEST_DIR="${EXES_DIR}/arm-simple"
    local MAIN_EXE="${TEST_DIR}/main.reference"
    local PATCH_EXE="${TEST_DIR}/main.patched.reference"
    local TEST_PATCH_EXE="${TEST_DIR}/test.patched.by.vibes"

    print_header "Checking ${TEST_DIR}"

    # Check the precompiled executables.
    run_arm_exe "${MAIN_EXE}" 5
    run_arm_exe "${PATCH_EXE}" 3

    # Check that vibes patches correctly.
    run_make "make clean -C ${TEST_DIR}" 0
    run_make "make patch.reference -C ${TEST_DIR}" 0
    run_arm_exe "${TEST_PATCH_EXE}" 3
}

test_arm_simple_cegis () {
    local TEST_DIR="${EXES_DIR}/arm-simple-cegis"
    local MAIN_EXE="${TEST_DIR}/main.reference"
    local PATCH_EXE="${TEST_DIR}/main.patched.reference"
    local TEST_PATCH_EXE="${TEST_DIR}/test.patched.by.vibes"

    print_header "Checking ${TEST_DIR}"

    # Check the precompiled executables.
    run_arm_exe "${MAIN_EXE}" 5
    run_arm_exe "${PATCH_EXE}" 1

    # Check that vibes patches correctly.
    run_make "make clean -C ${TEST_DIR}" 0
    run_make "make patch.reference -C ${TEST_DIR}" 0
    run_arm_exe "${TEST_PATCH_EXE}" 1
}

test_arm_simple_compiled () {
    local TEST_DIR="${EXES_DIR}/arm-simple-compiled"
    local MAIN_EXE="${TEST_DIR}/main.reference"
    local PATCH_EXE="${TEST_DIR}/main.patched.reference"
    local TEST_PATCH_EXE="${TEST_DIR}/test.patched.by.vibes"

    print_header "Checking ${TEST_DIR}"

    # Check the precompiled executables.
    run_arm_exe "${MAIN_EXE}" 5
    run_arm_exe "${PATCH_EXE}" 3

    # Check that vibes patches correctly.
    run_make "make clean -C ${TEST_DIR}" 0
    run_make "make patch.reference -C ${TEST_DIR}" 0
    run_arm_exe "${TEST_PATCH_EXE}" 3
}

test_arm_simple_multi () {
    local TEST_DIR="${EXES_DIR}/arm-simple-multi"
    local MAIN_EXE="${TEST_DIR}/main.reference"
    local PATCH_EXE="${TEST_DIR}/main.patched.reference"
    local TEST_PATCH_EXE="${TEST_DIR}/test.patched.by.vibes"

    print_header "Checking ${TEST_DIR}"

    # Check the precompiled executables.
    run_arm_exe "${MAIN_EXE}" 0
    run_arm_exe "${PATCH_EXE}" 1

    # Check that vibes patches correctly.
    run_make "make clean -C ${TEST_DIR}" 0
    run_make "make patch.reference -C ${TEST_DIR}" 0
    run_arm_exe "${TEST_PATCH_EXE}" 1
}

test_arm_subst_reg () {
    local TEST_DIR="${EXES_DIR}/arm-subst-reg"
    local MAIN_EXE="${TEST_DIR}/main.reference"
    local PATCH_EXE="${TEST_DIR}/main.patched.reference"
    local TEST_PATCH_EXE="${TEST_DIR}/test.patched.by.vibes"

    print_header "Checking ${TEST_DIR}"

    # Check the precompiled executables.
    run_arm_exe "${MAIN_EXE}" 5
    run_arm_exe "${PATCH_EXE}" 3

    # Check that vibes patches correctly.
    run_make "make clean -C ${TEST_DIR}" 0
    run_make "make patch.reference -C ${TEST_DIR}" 0
    run_arm_exe "${TEST_PATCH_EXE}" 3
}

test_arm_subst_stack () {
    local TEST_DIR="${EXES_DIR}/arm-subst-stack"
    local MAIN_EXE="${TEST_DIR}/main.reference"
    local PATCH_EXE="${TEST_DIR}/main.patched.reference"
    local TEST_PATCH_EXE="${TEST_DIR}/test.patched.by.vibes"

    print_header "Checking ${TEST_DIR}"

    # Check the precompiled executables.
    run_arm_exe "${MAIN_EXE}" 5
    run_arm_exe "${PATCH_EXE}" 3

    # Check that vibes patches correctly.
    run_make "make clean -C ${TEST_DIR}" 0
    run_make "make patch.reference -C ${TEST_DIR}" 0
    run_arm_exe "${TEST_PATCH_EXE}" 3
}

# Run these tests:
test_arm_simple
test_arm_simple_cegis
test_arm_simple_compiled
test_arm_simple_multi
test_arm_subst_reg
test_arm_subst_stack
