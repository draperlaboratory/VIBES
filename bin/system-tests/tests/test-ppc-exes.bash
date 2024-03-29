# --------------------------------------------------------------
#
# This script tests the "ppc-*" exes.
#
# --------------------------------------------------------------

test_ppc_bounds_check () {
    local TEST_DIR="${EXES_DIR}/ppc-bounds-check"
    local MAIN_EXE="${TEST_DIR}/main.reference"
    local PATCH_EXE="${TEST_DIR}/main.patched.reference"
    local TEST_PATCH_EXE="${TEST_DIR}/main.patched"

    print_header "Checking ${TEST_DIR}"

    run_make "make clean -C ${TEST_DIR}" 0

    # Check the precompiled executables.
    run_make "make main -C ${TEST_DIR}" 0
    run_make "make main.patched.reference -C ${TEST_DIR}" 0
    run_ppc_exe "${MAIN_EXE}" 80
    run_ppc_exe "${PATCH_EXE}" 255

    # Check that vibes patches correctly.
    run_make "make main.patched -C ${TEST_DIR}" 0
    run_ppc_exe "${TEST_PATCH_EXE}" 255
}

test_ppc_subst_stack () {
    local TEST_DIR="${EXES_DIR}/ppc-subst-stack"
    local MAIN_EXE="${TEST_DIR}/main.reference"
    local PATCH_EXE="${TEST_DIR}/main.patched.reference"
    local TEST_PATCH_EXE="${TEST_DIR}/test.patched.by.vibes"

    print_header "Checking ${TEST_DIR}"

    # Check the precompiled executables.
    run_ppc_exe "${MAIN_EXE}" 5
    run_ppc_exe "${PATCH_EXE}" 3

    # Check that vibes patches correctly.
    run_make "make clean -C ${TEST_DIR}" 0
    run_make "make patch.reference -C ${TEST_DIR}" 0
    run_ppc_exe "${TEST_PATCH_EXE}" 3
}

test_ppc_null_check () {
    local TEST_DIR="${EXES_DIR}/ppc-null-check"
    local MAIN_EXE="${TEST_DIR}/main"
    local PATCH_EXE="${TEST_DIR}/main.patched.reference"
    local TEST_PATCH_EXE="${TEST_DIR}/main.patched"

    print_header "Checking ${TEST_DIR}"

    run_make "make clean -C ${TEST_DIR}" 0

    # Check the precompiled executables.
    run_make "make main -C ${TEST_DIR}" 0
    run_make "make main.patched.reference -C ${TEST_DIR}" 0
    run_ppc_exe "${MAIN_EXE}" 5
    run_ppc_exe "${PATCH_EXE}" 5

    # Check that vibes patches correctly.
    run_make "make main.patched -C ${TEST_DIR}" 0
    run_ppc_exe "${TEST_PATCH_EXE}" 5
}

test_ppc_patch_call () {
    local TEST_DIR="${EXES_DIR}/ppc-patch-call"
    local MAIN_EXE="${TEST_DIR}/main.reference"
    local PATCH_EXE="${TEST_DIR}/main.patched.reference"
    local TEST_PATCH_EXE="${TEST_DIR}/test.patched.by.vibes"

    print_header "Checking ${TEST_DIR}"

    # Check the precompiled executables.
    run_ppc_exe "${MAIN_EXE}" 5
    run_ppc_exe "${PATCH_EXE}" 3

    # Check that vibes patches correctly.
    run_make "make clean -C ${TEST_DIR}" 0
    run_make "make patch.reference -C ${TEST_DIR}" 0
    run_ppc_exe "${TEST_PATCH_EXE}" 3
}

test_ppc_struct_ref () {
    local TEST_DIR="${EXES_DIR}/ppc-struct-ref"
    local MAIN_EXE="${TEST_DIR}/main.reference"
    local PATCH_EXE="${TEST_DIR}/main.patched.reference"
    local TEST_PATCH_EXE="${TEST_DIR}/main.patched"

    print_header "Checking ${TEST_DIR}"

    run_make "make clean -C ${TEST_DIR}" 0

    # Check the precompiled executables.
    run_make "make main -C ${TEST_DIR}" 0
    run_make "make main.patched.reference -C ${TEST_DIR}" 0
    run_ppc_exe "${MAIN_EXE}" 5
    run_ppc_exe "${PATCH_EXE}" 3

    # Check that vibes patches correctly.
    run_make "make main.patched -C ${TEST_DIR}" 0
    run_ppc_exe "${TEST_PATCH_EXE}" 3
}

# Run one test
run_test() {
    local FOLDERNAME=$1
    local ORIGVAL=$2
    local PATCHVAL=$3
    local TEST_DIR="${EXES_DIR}/${FOLDERNAME}"
    local MAIN_EXE="${TEST_DIR}/main.reference"
    local PATCH_EXE="${TEST_DIR}/main.patched.reference"
    local TEST_PATCH_EXE="${TEST_DIR}/main.patched"

    print_header "Checking ${TEST_DIR}"

    # Check the precompiled executables.
    run_make "make main -C ${TEST_DIR}" 0
    run_make "make main.patched.reference -C ${TEST_DIR}" 0
    run_ppc_exe "${MAIN_EXE}" $ORIGVAL
    run_ppc_exe "${PATCH_EXE}" $PATCHVAL

    # Check that vibes patches correctly.
    run_make "make clean -C ${TEST_DIR}" 0
    run_make "make main.patched -C ${TEST_DIR}" 0
    run_ppc_exe "${TEST_PATCH_EXE}" $PATCHVAL
}

# Run all tests
run_all () {
    test_ppc_bounds_check
    test_ppc_subst_stack
    test_ppc_null_check
    test_ppc_patch_call
    test_ppc_struct_ref
}
