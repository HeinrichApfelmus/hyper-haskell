#!/bin/sh
CONVERT=sips
echo ${CONVERT}
mkdir MyIcon.iconset
${CONVERT} -z 16 16     "$1" --out MyIcon.iconset/icon_16x16.png
${CONVERT} -z 32 32     "$1" --out MyIcon.iconset/icon_16x16@2x.png
${CONVERT} -z 32 32     "$1" --out MyIcon.iconset/icon_32x32.png
${CONVERT} -z 64 64     "$1" --out MyIcon.iconset/icon_32x32@2x.png
${CONVERT} -z 128 128   "$1" --out MyIcon.iconset/icon_128x128.png
${CONVERT} -z 256 256   "$1" --out MyIcon.iconset/icon_128x128@2x.png
${CONVERT} -z 256 256   "$1" --out MyIcon.iconset/icon_256x256.png
${CONVERT} -z 512 512   "$1" --out MyIcon.iconset/icon_256x256@2x.png
${CONVERT} -z 512 512   "$1" --out MyIcon.iconset/icon_512x512.png
${CONVERT} -z 1024 1024 "$1" --out MyIcon.iconset/icon_512x512@2x.png
iconutil -c icns MyIcon.iconset
# rm MyIcon.iconset/*.png && rmdir MyIcon.iconset