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

if [ "$2" != "" ]; then
    echo "Initial date, Final Date", $2, $3
else
    echo "Provide initial and final date, format YYYY-MM-DD"
    exit 1
fi
inidate=$2
enddate=$3
enddate_p1=$(date -I -d "$enddate + 1 day")
d=$inidate
i=0
while [ "${d}" != "${enddate_p1}" ]; do 
  count=$(printf "%03d" $i)
  echo $d, $count, $i
  file_date=$basename$d".jpg"
  basenameanim=$basename"anim"$inidate"_"$enddate
  file_ind=$basenameanim"_"$count".jpg"
  
  if [ -f "$file_date" ]; then
    echo "$file_date : $file_ind"
    cp $file_date $file_ind
  else
    echo "cannot reach " $file_date
    exit 1
  fi
  d=$(date -I -d "$d + 1 day")
  ((i+=1))
  
done

#now create video

#Create video
#ffmpeg -framerate 1 -i $name"-%03d".png -c:v libx264 -profile:v high -crf 20 -pix_fmt yuv420p $name.mp4
ffmpeg -f image2 -framerate 1 -i $basenameanim"_"%3d.jpg -qscale:v 2 -crf 20 -pix_fmt yuv420p -vf "pad=ceil(iw/2)*2:ceil(ih/2)*2" $basenameanim.avi
ffmpeg -i $basenameanim.avi  -c:v libx264 -profile:v baseline -level 3.0 -pix_fmt yuv420p -vf "pad=ceil(iw/2)*2:ceil(ih/2)*2"  $basenameanim.mp4

rm $basenameanim*.jpg
