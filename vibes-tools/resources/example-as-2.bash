THIS_SCRIPT="${BASH_SOURCE[0]}"
THIS_DIR="$(cd "$(dirname "${THIS_SCRIPT}")" && pwd)"
ROOT_DIR="$(cd "${THIS_DIR}/.." && pwd)"

cd "${ROOT_DIR}"

vibes-as \
  --target bap:arm \
  --language llvm-armv7 \
  --vir-filepath resources/patch.2.vir \
  --vir-outfile resources/patch.2.as.vir \
  --model ../resources/minizinc/model.mzn  \
  --verbose
