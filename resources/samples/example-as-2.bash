THIS_SCRIPT="${BASH_SOURCE[0]}"
THIS_DIR="$(cd "$(dirname "${THIS_SCRIPT}")" && pwd)"
ROOT_DIR="$(cd "${THIS_DIR}/.." && pwd)"

cd "${ROOT_DIR}"

vibes-as \
  --target bap:armv7+le \
  --language llvm-armv7 \
  --vir-filepath samples/patch.2.vir \
  --asm-outfile samples/patch.2.asm \
  --model minizinc/model.mzn  \
  --patch-info-filepath samples/patch-info.2.json \
  --verbose
