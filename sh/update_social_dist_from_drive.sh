#!/bin/bash
source ~/anaconda3/etc/profile.d/conda.sh
conda activate mdyn
cd /home/pedrosp/mdyn
dir=`pwd`
echo $dir
echo "Cities:"
rclone copy drive:"Social Distancing Index by Cities" --drive-shared-with-me data/isol_br/.
echo " ...done."
echo "States:"
rclone copy drive:"Social Distancing Index by States" --drive-shared-with-me data/isol_br/.
echo "...done."
echo "Microreg:"
rclone copy drive:"Social Distance Index by Micro Region.csv" --drive-shared-with-me data/isol_br/.
echo "...done."

echo
echo "Fixing city names:"
cd data/isol_br/
date=`date -d "yesterday 13:00" '+%Y-%m-%d'`
echo $date
cp "Social Distancing Index by Cities" "Social Distancing Index by Cities"$date".csv"
cp "Social Distancing Index by States" "Social Distancing Index by States"$date".csv"
cp "Social Distance Index by Micro Region.csv" "Social Distancing Index by Micro Region"$date".csv"
cd $dir

python mdynpy/mdyn_fix_inloco_cities.py  data/isol_br/"Social Distancing Index by Cities"$date".csv" maps/br_municipios/BRMUE250GC_SIR.shp


echo "Generating maps"
#awk 'NR==1 {$0="dia <-" replace} 1' replace=\"$date\" generate_isolationMap.R >   generate_isolationMap.R
rfile=Rcodes/generate_isolationMap.R
awk '{ if ( NR == 1 ) { print "dia <-" replace ;} else {print $0;} }' replace=\"$date\" $rfile >   generate_isolationMap$date.R

Rscript generate_isolationMap$date.R

rsync -av html/* pedrosp@ime.usp.br:www/covid19-data/iso_index/.

sleep 10m

rsync -av html/* pedrosp@ime.usp.br:www/covid19-data/iso_index/.

mv generate_isolationMap$date.R Rcodes/.
