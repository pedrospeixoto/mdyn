#!/bin/bash

echo "Converting pdf to png..."

cd /storage/SEIR/$1

#Convert files in main directory and delete pdf
mogrify -density 100 -format png ./*.pdf;
find . -maxdepth 1 -type f -iname "*.pdf" -delete;
find . -maxdepth 1 -type f -iname "*.png" -exec cp {} /storage/ShinyApps/seircovid19/www/ \; &

#Convert files in AjusteRate
mogrify -density 100 -format png ./AjusteRate/*.pdf;
find ./AjusteRate/ -maxdepth 1 -type f -iname "*.pdf" -delete;
find . -maxdepth 1 -type f -iname "*.png" -exec cp {} /storage/ShinyApps/seircovid19/www/ \; &

#Convert files in EPCurve
mogrify -density 100 -format png ./EPCurve/*.pdf;
find .EPCurve/ -maxdepth 1 -type f -iname "*.pdf" -delete;
find . -maxdepth 1 -type f -iname "*.png" -exec cp {} /storage/ShinyApps/seircovid19/www/ \; &

#Convert files in validate
mogrify -density 100 -format png ./validate/*.pdf;
find .EPCurve/ -maxdepth 1 -type f -iname "*.pdf" -delete;
find . -maxdepth 1 -type f -iname "*.png" -exec cp {} /storage/ShinyApps/seircovid19/www/ \; &

#Convert files in videos and create video
for d in "Videos"/*     # list directories in the form "/tmp/dirname/"
do
  #Convert files in casos
  mogrify -density 100 -format png ./Videos/$d/casos/*.pdf;
  find ./Videos/$d/casos/ -maxdepth 1 -type f -iname "*.pdf" -delete;
  ffmpeg -framerate 1 -i "%03d".png -c:v libx264 -profile:v high -crf 20 -pix_fmt yuv420p /storage/SEIR/$1/casos_$d_$1.mp4 &

  #Convert files in mortes
  mogrify -density 100 -format png ./Videos/$d/mortes/*.pdf;
  find ./Videos/$d/mortes/ -maxdepth 1 -type f -iname "*.pdf" -delete;
  ffmpeg -framerate 1 -i "%03d".png -c:v libx264 -profile:v high -crf 20 -pix_fmt yuv420p /storage/SEIR/$1/mortes_$d_$1.mp4 &
done

#Copy video files to ShinyApp
find . -maxdepth 1 -type f -iname "*.mp4" -exec cp {} /storage/ShinyApps/seircovid19/www/ \;
