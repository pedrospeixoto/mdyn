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
find ./EPCurve/ -maxdepth 1 -type f -iname "*.pdf" -delete;
find . -maxdepth 1 -type f -iname "*.png" -exec cp {} /storage/ShinyApps/seircovid19/www/ \; &

#Convert files in validate
mogrify -density 100 -format png ./validate/*.pdf;
find ./validate/ -maxdepth 1 -type f -iname "*.pdf" -delete;
find . -maxdepth 1 -type f -iname "*.png" -exec cp {} /storage/ShinyApps/seircovid19/www/ \; &

#Convert files to png
cd /storage/SEIR/$1/Videos
for d in *     # list directories
do
  #Convert files in casos
  mogrify -density 100 -format png ./$d/casos/*.pdf &

  #Convert files in mortes
  mogrify -density 100 -format png ./$d/mortes/*.pdf &
done
return 1
