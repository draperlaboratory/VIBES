THIS_SCRIPT="${BASH_SOURCE[0]}"
THIS_DIR="$(cd "$(dirname "${THIS_SCRIPT}")" && pwd)"
ROOT_DIR="$(cd "${THIS_DIR}/.." && pwd)"

cd "${ROOT_DIR}"

vibes-opt \
  --target bap:armv7+le \
  --language llvm-armv7 \
  --patch-info-filepath samples/patch-info.json \
  --bir-filepath samples/patch.bir \
  --bir-outfile samples/patch.opt.bir \
  --verbose
