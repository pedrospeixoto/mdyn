#!/bin/bash

echo "Convertendo pdf para png"
echo "----------------------"

fig_dir="plots"

cd $fig_dir
ls
for x in {A..Z}
do
    echo "$x"
    mogrify -verbose -density 100 -format png ./isol_$x*.pdf &    
done
