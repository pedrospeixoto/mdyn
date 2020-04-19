#!/bin/bash

echo "Processando Relatorios"
echo "----------------------"

fig_dir="Figuras"
relat_dir="Relatórios"
main_relat_dir="Relatórios"
html_s1="<li><a href="\""http://www.ime.usp.br/~pedrosp/covid19-data/iso_index/"
html_s2=" target="\""_blank"\"" rel="\""noopener"\"">"
html_s3="</a></li>"
long_name="Relatório sobre isolamento durante a pandemia.pdf"

for dir in "Workspace"/*    
do
    dir=${dir%*/}
    name=${dir##*/}
    echo $name $dir

    cd $dir
    
    base=`pwd`
    site=$base"/site.txt"
    echo $site
    echo "site" > "${site}"
    cd $relat_dir

    path1=$name/$relat_dir
    #file patter name
    for relat in *.pdf; do
	label="${relat#$name_sp}"
	label=${label%"$long_name"}
	label=`echo $label | sed -r 's/_/ - /g'`
	label=$label" (pdf)"
	echo $label
	echo $html_s1$path1/${relat}"\""$html_s2 $label $html_s3 >> "${site}" 
    done

done
