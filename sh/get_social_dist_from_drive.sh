#!/bin/bash
dir=`pwd`
echo $dir
echo "Cities:"
rclone copy drive:"Social Distancing Index by States" --drive-shared-with-me data/isol_br/.
echo " ...done."
echo "States:"
rclone copy drive:"Social Distancing Index by States" --drive-shared-with-me data/isol_br/.
echo "...done."
echo "Microreg:"
rclone copy drive:"Social Distance Index by Micro Region.csv" --drive-shared-with-me data/isol_br/.
echo "...done."
