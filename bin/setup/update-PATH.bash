# Make sure this is bash.
if [ -z "${BASH_VERSION}" ]; then
    1>2& echo "No BASH_VERSION found. Use bash to run this script."
    exit 1
fi

# Define some paths.
THIS_SCRIPT="${BASH_SOURCE[0]}"
THIS_DIR="$(cd "$(dirname "${THIS_SCRIPT}")" && pwd)"
COMMON_LIB_DIR="$(cd "${THIS_DIR}/../common-lib" && pwd)"

# Include the relevant data.
. "${THIS_DIR}/paths.minizinc"
. "${THIS_DIR}/paths.boolector"

# Add the minizinc and boolector locations to the PATH
export PATH="${LOCAL_MINIZINC_BIN_DIR}":"${LOCAL_BOOLECTOR_BIN_DIR}":"${PATH}"
