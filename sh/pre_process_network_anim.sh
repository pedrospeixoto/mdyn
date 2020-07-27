#!/bin/bash

echo "Gerando imagens para videos"
echo "---------------------------"


if [ "$1" != "" ]; then
    echo "Parameter file", $1
else
    echo "Provide a parameter file without extension"
    exit 1
fi
file=$1

if [ "$2" != "" ]; then
    echo "Final Date", $2
else
    echo "Provide initial and final date, format YYYY-MM-DD"
    exit 1
fi
inidate="2020-02-01"
enddate=$2



inimonth=$(date -d "$inidate" '+%m')
i=$(($inimonth))
endmonth=$(date -d "$enddate" '+%m')
endmonth=$(($endmonth+1))

while [ $i != $endmonth ]; do 
  month=$(printf "%02d" $i)
  
  ini_date="2020-"$month"-01"
  mp1=$(($month+1))
  mp1=$(printf "%02d" $mp1)
  if [ $mp1 != $endmonth ]; then
    end_date="2020-"$mp1"-01"
  else 
    end_date=$enddate
  fi
  echo $month $ini_date $end_date
  
  awk '{ if ( NR == 31 ) { print "date_ini = " replace ;} else {print $0;} }' replace=\"$ini_date\" $file".txt" >   $file$month".tmp"
  awk '{ if ( NR == 32 ) { print "date_end = " replace ;} else {print $0;} }' replace=\"$end_date\" $file$month".tmp" >   $file$month".txt"
  rm $file$month".tmp"
  #awk '{ if ( NR == 1 ) { print "dia <-" replace ;} else {print $0;} }' replace=\"$date\" $file >   "tmp_"$file
  nohup python -u mdyn_run.py -f $file$month".txt" -o 1 &> log_$month.txt &
  rm $file$month".txt"
  ((i+=1))
done



