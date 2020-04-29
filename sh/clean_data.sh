#!/bin/bash
#clean diagnostics and analysis
#Arument is data dir


if [ "$1" != "" ]; then
    echo "Deleting files from this directory", $1
else
    echo "Provide a directory as input parameters"
    exit 1
fi

#Clean .csv, .pkl, .eps files
HERE=`pwd`
DIR=$1

cd $DIR

find . -name '*.csv' -type f #-delete
find . -name '*.eps' -type f #-delete
find . -name '*.jpg' -type f #-delete
#find . -name '*.pkl' -type f #-delete

echo "Are you sure you want to delete these files?0/1"
read del

if (( $del )); then
    find . -name '*.csv' -type f -delete
    find . -name '*.eps' -type f -delete
    find . -name '*.jpg' -type f -delete
    #find . -name '*.pkl' -type f -delete
    echo "All gone!"
fi

cd $HERE
