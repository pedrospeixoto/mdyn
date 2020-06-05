#!/bin/bash
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

#Copy video files to ShinyApp
cd /storage/SEIR/$1
find . -maxdepth 1 -type f -iname "*.mp4" -exec cp {} /storage/ShinyApps/seircovid19/www/ \;
cd /storage/SEIR/$1/Videos/Estado
cp -r casos/ /storage/ShinyApps/seircovid19/www/
cp -r mortes/ /storage/ShinyApps/seircovid19/www/

echo "Syncing with Shiny server..."

rsync -u -avz -e "ssh -p 2223" dmarcondes@shiny.ime.usp.br:ShinyApps /storage/
rsync -u -avz -e "ssh -p 2223" /storage/ShinyApps dmarcondes@shiny.ime.usp.br:
