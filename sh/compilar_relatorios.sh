#!/bin/bash

echo "Processando Relatorios"
echo "----------------------"

fig_dir="Figuras"
relat_dir="Relatorios"
main_relat_dir="Relatórios"
base=`pwd`
ap=$base/$main_relat_dir/apendice.tex
bkp=$base/bkp

for dir in "Workspace"/* 
do
    dir=${dir%*/}   
    echo ${dir##*/}

    #Entrar no diretorio do estado
    cd $dir
    
    #Copiar arquivos para aa pasta
    cp "${ap}" $relat_dir/.
    cp -r "${fig_dir}" "${relat_dir}"/.

    #Entre no diretorio de relatorios
    cd "${relat_dir}"
    echo `pwd`

    #Compilar latex
    latexmk -interaction=nonstopmode -pdf -f -quiet

    #Limpar
    #rf -f #$bkp/.
    rm -f *.fls
    #rm -f *.tex #$bkp/.
    rm -f *.aux
    rm -f *.log
    rm -f *.fdb_latexmk

    #Sair do diretorio de relatorios e estado
    cd "${base}"
    echo `pwd`
done
