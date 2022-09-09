THIS_SCRIPT="${BASH_SOURCE[0]}"
THIS_DIR="$(cd "$(dirname "${THIS_SCRIPT}")" && pwd)"
ROOT_DIR="$(cd "${THIS_DIR}/.." && pwd)"

cd "${ROOT_DIR}"

vibes-as \
  --target bap:arm \
  --language llvm-armv7 \
  --vir-filepath resources/patch.vir \
  --asm-outfile resources/patch.asm \
  --model ../resources/minizinc/model.mzn  \
  --verbose
