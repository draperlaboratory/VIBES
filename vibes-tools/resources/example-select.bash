THIS_SCRIPT="${BASH_SOURCE[0]}"
THIS_DIR="$(cd "$(dirname "${THIS_SCRIPT}")" && pwd)"
ROOT_DIR="$(cd "${THIS_DIR}/.." && pwd)"

cd "${ROOT_DIR}"

vibes-select \
  --target bap:armv7+le \
  --language llvm-armv7 \
  --patch-info-filepath resources/patch-info.json \
  --bir-filepath resources/patch.opt.bir \
  --vir-outfile resources/patch.vir \
  --verbose
