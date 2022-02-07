# --------------------------------------------------------------
#
# Cut a new release
#
# --------------------------------------------------------------

# Define some paths.
THIS_SCRIPT="${BASH_SOURCE[0]}"
THIS_DIR="$(cd "$(dirname "${THIS_SCRIPT}")" && pwd)"
COMMON_LIB_DIR="$(cd "${THIS_DIR}/../common-lib" && pwd)"

# Include the relevant libraries.
. "${COMMON_LIB_DIR}/utils.bash"

# OPAM files
LIB_OPAM_FILE="${REPO_ROOT}/bap-vibes/bap-vibes.opam"
PLUGIN_OPAM_FILE="${REPO_ROOT}/plugin/vibes.opam"

# Get the package versions from an OPAM file
get_version () {
    FILE="${1}"
    grep -o '^version: ".*"' "${FILE}" | \
    grep -o '".*"' -- | \
    grep -o '[0-9+].[0-9+].[0-9+]' --
}

# Get the current git branch
current_branch () {
    git branch --show-current
}

# Confirm or exit. `${1}` is the confirmation message to display. 
confirm () {
    local MSG="${1}"
    while true; do
        echo -n "${1} " 
    read CONFIRM
    case "${CONFIRM}" in

        yes|Yes|YES|y|Y)
            break
            ;;

	no|No|NO|n|N)
            echo "Exiting."	
            exit 1
            ;;

	*)
            echo "Please answer yes or no."
            ;;

    esac
done
}

# Usage message
usage () {
    echo "USAGE: bash $(get_me) [OPTIONS]"
    echo ""
    echo "  Cut a new release."
    echo ""
    echo "OPTIONS"
    echo "  -h | --help       Print this help and exit"
}

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

# Make sure we're on main.
CURRENT_BRANCH="$(current_branch)"
if [[ "${CURRENT_BRANCH}" != "main" ]]; then
    confirm "You are not on the 'main' branch. Proceed anyway? (yes/no)"
fi

# Report the current version of the lib and plugin.
LIB_VERSION="$(get_version "${LIB_OPAM_FILE}")"
echo "Library (bap-vibes) version: ${LIB_VERSION}"
PLUGIN_VERSION="$(get_version "${PLUGIN_OPAM_FILE}")"
echo "Plugin (vibes) version: ${PLUGIN_VERSION}"

# Get the version we want to bump up to.
echo ""
echo -n "Enter new version to bump to: "
read NEW_VERSION

# Confirm
confirm "About to bump to version ${NEW_VERSION}. Confirm? (yes/no)"

# The release branch name.
RELEASE_BRANCH="release-${NEW_VERSION}"

# Update the version numbers in the *.opam files.
sed "s/^version: \".*\"/version: \"${NEW_VERSION}\"/" "${LIB_OPAM_FILE}" > "${LIB_OPAM_FILE}.bak"
sed "s/^version: \".*\"/version: \"${NEW_VERSION}\"/" "${PLUGIN_OPAM_FILE}" > "${PLUGIN_OPAM_FILE}.bak"
mv "${LIB_OPAM_FILE}.bak" "${LIB_OPAM_FILE}"
mv "${PLUGIN_OPAM_FILE}.bak" "${PLUGIN_OPAM_FILE}"
git add "${LIB_OPAM_FILE}"
git add "${PLUGIN_OPAM_FILE}"
git commit -m "Bump version to ${NEW_VERSION}"
BUMP_STATUS="${?}"

# Exit if we had problems.
if [[ "${BUMP_STATUS}" != "0" ]]; then
    echo "Failed to bump version. Exiting."
    exit 1
fi

# Create a release branch.
git checkout -b "${RELEASE_BRANCH}"

# Push to remote.
git push origin "${RELEASE_BRANCH}"
PUSH_STATUS="${?}"

# Exit if we had problems.
if [[ "${PUSH_STATUS}" != "0" ]]; then
    echo "Failed to push release branch. Exiting."
    exit 1
fi

# Cleanup and go back to the branch we were on before.
git checkout "${CURRENT_BRANCH}"
git branch -D "${RELEASE_BRANCH}"
CLEANUP_STATUS="${?}"

# Exit if we had problems.
if [[ "${CLEANUP_STATUS}" != "0" ]]; then
    echo "Failed to delete local copy of the release branch. Exiting."
    exit 1
fi

# If we made it here, everything worked.
echo "Release ${NEW_VERSION} was cut."
