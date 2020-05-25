#!/bin/bash
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

