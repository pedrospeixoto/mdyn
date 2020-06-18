#!/bin/bash
#update Shiny apps

#Clear ShinyApps
find /home/dmarcondes/GDrive/mdyn/ShinyApps/seircovid19 -type f -iname '*.png' -delete;
find /home/dmarcondes/GDrive/mdyn/ShinyApps/seircovid19 -type f -iname '*.csv' -delete;
find /home/dmarcondes/GDrive/mdyn/ShinyApps/seircovid19 -type f -iname '*.rds' -delete;
find /home/dmarcondes/GDrive/mdyn/ShinyApps/seircovid19 -type f -iname '*.mp4' -delete;

rsync -u -v --stats --progress -avz -e "ssh -p 2223" /home/dmarcondes/GDrive/mdyn/ShinyApps dmarcondes@shiny.ime.usp.br:
rsync -u -v --stats --progress -avz -e "ssh -p 2223" dmarcondes@shiny.ime.usp.br:ShinyApps /home/dmarcondes/GDrive/mdyn/
