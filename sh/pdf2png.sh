#!/bin/bash

echo "Convertendo pdf para png"
echo "----------------------"

html="html"
fig_dir="plots"
cd $html
cd $fig_dir
for x in {A..Z}
do
    mogrify -verbose -density 100 -format png ./isol_$x*.pdf &    
done
