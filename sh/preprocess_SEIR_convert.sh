#!/bin/bash

echo "Converting pdf to png..."

cd /storage/SEIR/$1

#Clear ShinyApps
find /storage/ShinyApps/seircovid19 -type f -iname '*.png' -delete;
find /storage/ShinyApps/seircovid19 -type f -iname '*.csv' -delete;
find /storage/ShinyApps/seircovid19 -type f -iname '*.mp4' -delete;

#Convert files in main directory and delete pdf
mogrify -density 100 -format png ./*.pdf;
find . -maxdepth 1 -type f -iname "*.pdf" -delete;
find . -maxdepth 1 -type f -iname "*.png" -exec cp {} /storage/ShinyApps/seircovid19/www/ \; &

#Convert files in AjusteRate
mogrify -density 100 -format png ./AjusteRate/*.pdf;
find ./AjusteRate/ -maxdepth 1 -type f -iname "*.pdf" -delete;
find ./AjusteRate/ -maxdepth 1 -type f -iname "*.png" -exec cp {} /storage/ShinyApps/seircovid19/www/ \; &

#Convert files in EPCurve
mogrify -density 100 -format png ./EPCurve/*.pdf;
find ./EPCurve/ -maxdepth 1 -type f -iname "*.pdf" -delete;
find ./EPCurve/ -maxdepth 1 -type f -iname "*.png" -exec cp {} /storage/ShinyApps/seircovid19/www/ \; &

#Convert files in validate
mogrify -density 100 -format png ./validate/*.pdf;
find ./validate/ -maxdepth 1 -type f -iname "*.pdf" -delete;
find ./validate/ -maxdepth 1 -type f -iname "*.png" -exec cp {} /storage/ShinyApps/seircovid19/www/ \; &

#Convert files to png
cd /storage/SEIR/$1/Videos
for d in *     # list directories
do
  #Convert files in casos
  mogrify -density 100 -format png ./$d/casos/*.pdf &

  #Convert files in mortes
  mogrify -density 100 -format png ./$d/mortes/*.pdf &
done;

#Create countdown 0f 30 minutes
function countdown(){
   date1=$((`date +%s` + $1));
   while [ "$date1" -ge `date +%s` ]; do
     echo -ne "$(date -u --date @$(($date1 - `date +%s`)) +%H:%M:%S)\r";
     sleep 0.1
   done
}

#countdown 600;

echo "Creating videos..."

cd /storage/SEIR/$1/Videos

for d in *     # list directories
do
  #Files in casos
  find ./$d/casos/ -maxdepth 1 -type f -iname "*.pdf" -delete;
  ffmpeg -framerate 5 -i "./$d/casos/%03d".png -c:v libx264 -profile:v high -crf 20 -pix_fmt yuv420p /storage/SEIR/$1/casos_$d.mp4 &

  #Files in mortes
  find ./$d/mortes/ -maxdepth 1 -type f -iname "*.pdf" -delete;
  ffmpeg -framerate 5 -i "./$d/mortes/%03d".png -c:v libx264 -profile:v high -crf 20 -pix_fmt yuv420p /storage/SEIR/$1/mortes_$d.mp4 &
done;

#countdown 600;

#Copy video files to ShinyApp
cd /storage/SEIR/$1
find . -maxdepth 1 -type f -iname "*.mp4" -exec cp {} /storage/ShinyApps/seircovid19/www/ \;
cd /storage/SEIR/$1/Videos/Estado
cp -r casos/ /storage/ShinyApps/seircovid19/www/
cp -r mortes/ /storage/ShinyApps/seircovid19/www/

echo "Syncing with Shiny server..."

ssh -p 2223 dmarcondes@shiny.ime.usp.br "{
  find ./ShinyApps/seircovid19 -type f -iname '*.png' -delete;
  find ./ShinyApps/seircovid19 -type f -iname '*.csv' -delete;
  find ./ShinyApps/seircovid19 -type f -iname '*.rds' -delete;
  find ./ShinyApps/seircovid19 -type f -iname '*.mp4' -delete;
}"
rsync -u -avz -e "ssh -p 2223" dmarcondes@shiny.ime.usp.br:ShinyApps /storage/
rsync -u -avz -e "ssh -p 2223" /storage/ShinyApps dmarcondes@shiny.ime.usp.br:
