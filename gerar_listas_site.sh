#!/bin/bash

echo "Processando Relatorios"
echo "----------------------"

work="workspace"
fig_dir="Figuras"
relat_dir="Relatorios"
main_relat_dir="Relatórios"
html_s0="<a href="\""http://www.ime.usp.br/~pedrosp/covid19-data/iso_index/"
html_s1="<li><a href="\""http://www.ime.usp.br/~pedrosp/covid19-data/iso_index/"
html_s2=" target="\""_blank"\"" rel="\""noopener"\"">"
html_s3="</a></li>"
html_s4="</a>"
long_name="Relatório sobre isolamento durante a pandemia.pdf"
texto="Cidades em que não há informações com confiança suficiente para calcular o Índice de Isolamento Social:"

for dir in "${work}"/*    
do
    dir=${dir%*/}
    name=${dir##*/}
    echo $name $dir

    cd $dir
    
    base=`pwd`
    site=$base"/site.txt"
    echo $site
    echo "" > "${site}"

    #entrar no diretorio de relatorios
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
    echo "</ul>" >> "${site}"

    #sai do diretorio de relatorios
    cd ..
    ls
    #municipios sem dados
    csvfile=$name"_cidades_sem_relatorio.csv"
    #cat $csvfile
    OLDIFS=$IFS
    IFS=','
    echo "" >> "${site}"
    echo $texto >> "${site}"
    echo "<ul>" >> "${site}"
    #[ ! -f $csvfile ] && { echo "$csvfile file not found"; exit 99; }
    while read id mun
    do
        if [ "$mun" != "\""problemas"\"" ]; then
            mun_s=`echo $mun | sed -r 's/"//g'`
            echo "<li>" "$mun_s" "</li>" >> "${site}"
        fi
    done <  "${csvfile}"
    IFS=$OLDIFS
    echo "</ul>" >> "${site}"

    exit 1
done
