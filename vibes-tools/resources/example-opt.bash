THIS_SCRIPT="${BASH_SOURCE[0]}"
THIS_DIR="$(cd "$(dirname "${THIS_SCRIPT}")" && pwd)"
ROOT_DIR="$(cd "${THIS_DIR}/.." && pwd)"

cd "${ROOT_DIR}"

vibes-opt \
  --target bap:arm \
  --language llvm-armv7 \
  --function-info-filepath resources/patch.func.info \
  --patch-info-filepath resources/patch-info.json \
  --bir-filepath resources/patch.bir \
  --bir-outfile resources/patch.opt.bir \
  --verbose
