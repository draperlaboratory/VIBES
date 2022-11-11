THIS_SCRIPT="${BASH_SOURCE[0]}"
THIS_DIR="$(cd "$(dirname "${THIS_SCRIPT}")" && pwd)"
ROOT_DIR="$(cd "${THIS_DIR}/.." && pwd)"

cd "${ROOT_DIR}"

vibes-select \
  --target bap:armv7+le \
  --language llvm-armv7 \
  --patch-info-filepath samples/patch-info.2.json \
  --bir-filepath samples/patch.2.opt.bir \
  --vir-outfile samples/patch.2.vir \
  --verbose
