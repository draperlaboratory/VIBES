THIS_SCRIPT="${BASH_SOURCE[0]}"
THIS_DIR="$(cd "$(dirname "${THIS_SCRIPT}")" && pwd)"
ROOT_DIR="$(cd "${THIS_DIR}/.." && pwd)"

cd "${ROOT_DIR}"

vibes-parse \
  --target bap:armv7+le \
  --patch-info-filepath resources/patch-info.json \
  --patch-filepath resources/patch.c \
  --bir-outfile resources/patch.bir \
  --function-info-outfile resources/patch.func.info \
  --verbose
