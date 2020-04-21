#!/bin/bash

echo "Limpando Relatorios"
echo "----------------------"

fig_dir="Figuras"
relat_dir="Relatorios"
main_relat_dir="Relatórios"
base=`pwd`

for dir in "Workspace"/* 
do
    dir=${dir%*/}   
    echo ${dir##*/}

    #Entrar no diretorio do estado
    cd $dir
    
    #Entre no diretorio de relatorios
    cd "${relat_dir}"
    echo `pwd`

    #Limpar
    #rm -rf "${fig_dir}" #$bkp/.
    rm -f *.fls
    rm -f *.tex #$bkp/.
    rm -f *.aux
    rm -f *.log
    rm -f *.fdb_latexmk

    #Sair do diretorio de relatorios e estado
    cd "${base}"
    echo `pwd`
done
