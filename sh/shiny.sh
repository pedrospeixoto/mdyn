#!/bin/bash
#update Shiny apps

rsync -u -v --stats --progress -avz -e "ssh -p 2223" /home/dmarcondes/GDrive/mdyn/ShinyApps dmarcondes@shiny.ime.usp.br:
rsync -u -v --stats --progress -avz -e "ssh -p 2223" dmarcondes@shiny.ime.usp.br:ShinyApps /home/dmarcondes/GDrive/mdyn/
