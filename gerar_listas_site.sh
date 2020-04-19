#!/bin/bash

echo "Processando Relatorios"
echo "----------------------"

fig_dir="Figuras"
relat_dir="Relatorios"
main_relat_dir="Relatórios"
html_s0="<a href="\""http://www.ime.usp.br/~pedrosp/covid19-data/iso_index/"
html_s1="<li><a href="\""http://www.ime.usp.br/~pedrosp/covid19-data/iso_index/"
html_s2=" target="\""_blank"\"" rel="\""noopener"\"">"
html_s3="</a></li>"
html_s4="</a>"
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
    echo "" > "${site}"
    cd $relat_dir

    path1=$name/$relat_dir

    #Cabecalho
    echo "<h3>"$name"</h3>" >> "${site}"
    echo "<strong>Índice no Estado:</strong>" >> "${site}"
    echo "" >>  "${site}"
    
    main_label=${long_name%".pdf"}
    echo $html_s0$path1/$name"_"$long_name"\""$html_s2 $main_label "-" $name $html_s4 >> "${site}"
    
    echo "" >>  "${site}"
    echo "<strong>Índice nos Municípios</strong>:" >> "${site}"
    echo "<ul>" >> "${site}"

    #Lista de municipios
    #file pattern name
    for relat in $name*.pdf; do
	label="${relat#$name}"
	label=${label%"$long_name"}
	label=`echo $label | sed -r 's/_/ - /g'`
	length=${#label}
	if [ ${#label} -ge 4 ]; then
	    label=`echo $label | sed -r 's/-/ /g'`
	    echo $label
	    echo $html_s1$path1/${relat}"\""$html_s2 $label $html_s3 >> "${site}"
	fi
    done
    echo "<ul>" >> "${site}"
    exit 1
done
