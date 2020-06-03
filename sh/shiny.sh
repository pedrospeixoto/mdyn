#!/bin/bash
#update Shiny apps

rsync -v -u --stats --progress -avz -e "ssh -p 2223" /home/dmarcondes/mdyn/ShinyApps dmarcondes@shiny.ime.usp.br:
