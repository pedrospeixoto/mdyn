#!/bin/bash
#update Shiny apps

rsync -v --stats --progress -avz -e "ssh -p 2223" /home/dmarcondes/mdyn/ShinyApps dmarcondes@shiny.ime.usp.br:
rsync -v --stats --progress -avz -e "ssh -p 2223" dmarcondes@shiny.ime.usp.br:ShinyApps /home/dmarcondes/mdyn/
