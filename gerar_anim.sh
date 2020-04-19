#!/bin/bash

echo "Gerando videos"
echo "----------------------"

fig_dir="Figuras"
day_ini=$(date +%s --date "2020-03-01")

for d in "Workspace"/*     # list directories in the form "/tmp/dirname/"
do
    d=${d%*/}
    
    #name of state
    name=${d##*/}
    
    echo $name    
    cd $d

    #file patter name
    filepat=$name"_2020"

    #go to figures directory
    cd $fig_dir
    
    for im in $filepat*.pdf; do
	echo $im

	#extrat date info and add counter
	day="${im#$name}"
	day="${day#_}"
	day=`basename $day .pdf`
	day=`echo $day | sed -r 's/_/-/g'`
	#i=$(date --date="${day} -${day_ini} day" +%Y-%m-%d)
	day=$(date --date=$day '+%s')
	difference=$(($day-$day_ini))
	count=$(($difference/(3600*24)))
	count=$(printf "%03d" $count)
	#echo $count

	#Convert to png with counter
	outname=$name"-"$count.png
	convert "$im" "$outname"
	echo $outname
    done
    #Create video
    ffmpeg -framerate 1 -i $name"-%03d".png -c:v libx264 -profile:v high -crf 20 -pix_fmt yuv420p $name.mp4
    rm -rf *.png
done
