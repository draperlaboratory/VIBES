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

test_arm_simple_inline () {
    local TEST_DIR="${EXES_DIR}/arm-simple-inline"
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

test_arm_vibes_loader () {
    local TEST_DIR="${EXES_DIR}/arm-vibes-loader"
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

test_arm_stripped_loader () {
    local TEST_DIR="${EXES_DIR}/arm-stripped-loader"
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

test_arm_patch_call () {
    local TEST_DIR="${EXES_DIR}/arm-patch-call"
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

test_arm_simple_ssa () {
    local TEST_DIR="${EXES_DIR}/arm-simple-ssa"
    local MAIN_EXE="${TEST_DIR}/main.reference"
    local PATCH_EXE="${TEST_DIR}/main.patched.reference"
    local TEST_PATCH_EXE="${TEST_DIR}/test.patched.by.vibes"

    print_header "Checking ${TEST_DIR}"

    # Check the precompiled executables.
    run_arm_exe "${MAIN_EXE}" 7
    run_arm_exe "${PATCH_EXE}" 3

    # Check that vibes patches correctly.
    run_make "make clean -C ${TEST_DIR}" 0
    run_make "make patch.reference -C ${TEST_DIR}" 0
    run_arm_exe "${TEST_PATCH_EXE}" 3
}

test_arm_linear_ssa () {
    local TEST_DIR="${EXES_DIR}/arm-linear-ssa"
    local MAIN_EXE="${TEST_DIR}/main.reference"
    local PATCH_EXE="${TEST_DIR}/main.patched.reference"
    local TEST_PATCH_EXE="${TEST_DIR}/test.patched.by.vibes"

    print_header "Checking ${TEST_DIR}"

    # Check the precompiled executables.
    run_arm_exe "${MAIN_EXE}" 15
    run_arm_exe "${PATCH_EXE}" 17

    # Check that vibes patches correctly.
    run_make "make clean -C ${TEST_DIR}" 0
    run_make "make patch.reference -C ${TEST_DIR}" 0
    run_arm_exe "${TEST_PATCH_EXE}" 3
}

test_arm_simple_sum () {
    local TEST_DIR="${EXES_DIR}/arm-simple-sum"
    local MAIN_EXE="${TEST_DIR}/main.reference"
    local PATCH_EXE="${TEST_DIR}/main.patched.reference"
    local TEST_PATCH_EXE="${TEST_DIR}/test.patched.by.vibes"

    print_header "Checking ${TEST_DIR}"

    # Check the precompiled executables.
    run_arm_exe "${MAIN_EXE}" 5
    run_arm_exe "${PATCH_EXE}" 9

    # Check that vibes patches correctly.
    run_make "make clean -C ${TEST_DIR}" 0
    run_make "make patch.reference -C ${TEST_DIR}" 0
    run_arm_exe "${TEST_PATCH_EXE}" 9
}

test_arm_branch () {
    local TEST_DIR="${EXES_DIR}/arm-branch"
    local MAIN_EXE="${TEST_DIR}/main.reference"
    local PATCH_EXE="${TEST_DIR}/main.patched.reference"
    local TEST_PATCH_EXE="${TEST_DIR}/main.patched"

    print_header "Checking ${TEST_DIR}"

    # Check the precompiled executables.
    run_make "make main -C ${TEST_DIR}" 0
    run_make "make main.patched.reference -C ${TEST_DIR}" 0
    run_arm_exe "${MAIN_EXE}" 1
    run_arm_exe "${PATCH_EXE}" 2

    # Check that vibes patches correctly.
    run_make "make clean -C ${TEST_DIR}" 0
    run_make "make main.patched -C ${TEST_DIR}" 0
    run_arm_exe "${TEST_PATCH_EXE}" 2
}

test_arm_branch_bsi () {
    local TEST_DIR="${EXES_DIR}/arm-branch/bsi"
    local MAIN_EXE="${TEST_DIR}/main.reference"
    local PATCH_EXE="${TEST_DIR}/main.patched.reference"
    local TEST_PATCH_EXE="${TEST_DIR}/main.patched"

    print_header "Checking ${TEST_DIR}"

    # Check the precompiled executables.
    run_make "make main -C ${TEST_DIR}" 0
    run_make "make main.patched.reference -C ${TEST_DIR}" 0
    run_arm_exe "${MAIN_EXE}" 1
    run_arm_exe "${PATCH_EXE}" 2

    # Check that vibes patches correctly.
    run_make "make clean -C ${TEST_DIR}" 0
    run_make "make main.patched -C ${TEST_DIR}" 0
    run_arm_exe "${TEST_PATCH_EXE}" 2
}

test_arm_isel_jmp () {
    local TEST_DIR="${EXES_DIR}/arm-isel-jmp"
    local MAIN_EXE="${TEST_DIR}/main.reference"
    local PATCH_EXE="${TEST_DIR}/main.patched.reference"
    local TEST_PATCH_EXE="${TEST_DIR}/main.patched"

    print_header "Checking ${TEST_DIR}"

    # Check the precompiled executables.
    run_make "make main -C ${TEST_DIR}" 0
    run_make "make main.patched.reference -C ${TEST_DIR}" 0
    run_arm_exe "${MAIN_EXE}" 5
    run_arm_exe "${PATCH_EXE}" 2

    # Check that vibes patches correctly.
    run_make "make clean -C ${TEST_DIR}" 0
    run_make "make main.patched -C ${TEST_DIR}" 0
    run_arm_exe "${TEST_PATCH_EXE}" 2
}

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
    run_arm_exe "${MAIN_EXE}" $ORIGVAL
    run_arm_exe "${PATCH_EXE}" $PATCHVAL

    # Check that vibes patches correctly.
    run_make "make clean -C ${TEST_DIR}" 0
    run_make "make main.patched -C ${TEST_DIR}" 0
    run_arm_exe "${TEST_PATCH_EXE}" $PATCHVAL
}

test_arm_isel_jmp2 () {
    run_test "arm-isel-jmp2" 5 6
}

test_arm_extra_constraints () {
    run_test "arm-extra-constraints" 5 3
}

# Run all tests
run_all () {
    test_arm_simple
    test_arm_extra_constraints
    test_arm_isel_jmp2
    test_arm_isel_jmp
    test_arm_simple_sum
    test_arm_simple_inline
    test_arm_simple_cegis
    test_arm_simple_compiled
    test_arm_simple_multi
    test_arm_subst_reg
    test_arm_subst_stack
    test_arm_vibes_loader
    test_arm_stripped_loader
    test_arm_branch
    test_arm_branch_bsi
}
