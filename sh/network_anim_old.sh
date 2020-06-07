#!/bin/bash

echo "Gerando videos "
echo "----------------------"


if [ "$1" != "" ]; then
    echo "Name pattern", $1
else
    echo "Provide a name pattern for video"
    exit 1
fi
basename=$1

ffmpeg -f image2 -framerate 1 -i $basename%3d.jpg -qscale:v 2 -crf 20 -pix_fmt yuv420p -vf "pad=ceil(iw/2)*2:ceil(ih/2)*2" $basename.avi
ffmpeg -i $basename.avi  -c:v libx264 -profile:v baseline -level 3.0 -pix_fmt yuv420p -vf "pad=ceil(iw/2)*2:ceil(ih/2)*2"  $basename.mp4


