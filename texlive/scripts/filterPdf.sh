#!/bin/bash -

# This script filters a PDF file and attempts to include as many fonts as possible.

# strict error handling
set -o pipefail  # trace ERR through pipes
set -o errtrace  # trace ERR through 'time command' and other functions
set -o nounset   # set -u : exit the script if you try to use an uninitialized variable
set -o errexit   # set -e : exit the script if any statement returns a non-true return value

echo "Filtering document '$1' in order to include as many fonts as possible."

source="$1"
dest="${source%%.*}.pdf"
tempDest="$(tempfile)"

echo "Filtering '$source' to temporary file '$tempDest'."

gs -q -dEmbedAllFonts=true \
      -dSubsetFonts=true \
      -dCompressFonts=true \
      -dOptimize=true \
      -dPreserveCopyPage=false \
      -dPreserveEPSInfo=false \
      -dPreserveHalftoneInfo=false \
      -dPreserveOPIComments=false \
      -dPreserveOverprintSettings=false \
      -dPreserveSeparation=false \
      -dPreserveDeviceN=false \
      -dMaxBitmap=2147483647 \
      -dDownsampleMonoImages=false \
      -dDownsampleGrayImages=false \
      -dDownsampleColorImages=false \
      -dDetectDuplicateImages=true \
      -dHaveTransparency=true \
      -dFastWebView=false \
      -dAutoFilterColorImages=false \
      -dAutoFilterGrayImages=false \
      -dColorImageFilter=/FlateEncode \
      -dGrayImageFilter=/FlateEncode \
      -dColorConversionStrategy=/LeaveColorUnchanged \
      -dPrinted=false \
      -dNOPAUSE \
      -dQUIET \
      -dBATCH \
      -dSAFER \
      -sDEVICE=pdfwrite \
      -dAutoRotatePages=/PageByPage \
      -sOutputFile="$tempDest" "$source" \
      -c ".setpdfwrite <</NeverEmbed [ ]>> setdistillerparams"

echo "Finished filtering '$source' to '$tempDest', now checking result."

if pdftotext "$tempDest" &> /dev/null; then
  echo "File '$tempDest' has survived a pdftotext check, so we will move it to '$dest'."
  mv "$tempDest" "$dest"
  chmod 777 "$dest" || true
else
  echo "Warning: '$tempDest' is broken according to pdftotext, so we will discard it."
  rm -f "$tempDest" || true
fi

echo "Done filtering document '$source'."
