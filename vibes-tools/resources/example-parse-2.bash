THIS_SCRIPT="${BASH_SOURCE[0]}"
THIS_DIR="$(cd "$(dirname "${THIS_SCRIPT}")" && pwd)"
ROOT_DIR="$(cd "${THIS_DIR}/.." && pwd)"

cd "${ROOT_DIR}"

vibes-parse \
  --target bap:armv7+le \
  --patch-info-filepath resources/patch-info.2.json \
  --patch-filepath resources/patch.2.c \
  --bir-outfile resources/patch.2.bir \
  --verbose
