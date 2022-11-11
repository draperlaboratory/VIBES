THIS_SCRIPT="${BASH_SOURCE[0]}"
THIS_DIR="$(cd "$(dirname "${THIS_SCRIPT}")" && pwd)"
ROOT_DIR="$(cd "${THIS_DIR}/.." && pwd)"

cd "${ROOT_DIR}"

vibes-parse \
  --target bap:armv7+le \
  --patch-info-filepath samples/patch-info.2.json \
  --patch-filepath samples/patch.2.c \
  --bir-outfile samples/patch.2.bir \
  --verbose
