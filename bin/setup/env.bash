# --------------------------------------------------------------
# 
# This script provides some environment variables
# used for running some of the tools.
#
# --------------------------------------------------------------

# Minizinc variables
MINIZINC_URL=https://github.com/MiniZinc/MiniZincIDE/releases/download/2.6.0/MiniZincIDE-2.6.0-bundle-linux-x86_64.tgz
MINIZINC_BUNDLE=MiniZincIDE-2.6.0-bundle-linux-x86_64
MINIZINC_DIR="${HOME}/${MINIZINC_BUNDLE}"
export PATH="${MINIZINC_DIR}/bin":"${PATH}"
export LD_LIBRARY_PATH="/lib/x86_64-linux-gnu/:/usr/lib/x86_64-linux-gnu/:${MINIZINC_DIR}/lib":"${LD_LIBRARY_PATH}"

# Boolector variables
BOOLECTOR_URL=https://github.com/boolector/boolector
BOOLECTOR_DIR="${HOME}/boolector"
export PATH="${BOOLECTOR_DIR}/build/bin":"${PATH}"
