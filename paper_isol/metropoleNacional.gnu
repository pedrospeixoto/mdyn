#ratio amog number of cases and its average 
set term postscript eps enhanced color font 'Helvetica,20'   #  16
set output "metropoleN-N.eps"
set encoding utf8
if (!exists("MP_LEFT"))   MP_LEFT = .15
if (!exists("MP_RIGHT"))  MP_RIGHT = .9
if (!exists("MP_BOTTOM")) MP_BOTTOM = .15
if (!exists("MP_TOP"))    MP_TOP = .9
if (!exists("MP_GAP"))    MP_GAP = 0.12
set multiplot layout 1,1  columnsfirst margins screen MP_LEFT, MP_RIGHT, MP_BOTTOM, MP_TOP spacing screen MP_GAP
set ylabel "{/*1.1{incidence/average group incidence}}"
set xlabel "{/*1.1{date}}"
a1 = 'pointtype 1 pointsize 0.5 linecolor rgb "blue"'
a2 = 'pointtype 2 pointsize 0.5 linecolor rgb "blue"'
a3 = 'pointtype 3 pointsize 0.5 linecolor rgb "blue"'
a4 = 'pointtype 4 pointsize 0.5 linecolor rgb "blue"'
a5 = 'pointtype 5 pointsize 0.5 linecolor rgb "blue"'
a6 = 'pointtype 6 pointsize 0.5 linecolor rgb "blue"'
a7 = 'pointtype 7 pointsize 0.5 linecolor rgb "blue"'
a8 = 'pointtype 8 pointsize 0.5 linecolor rgb "blue"'
a9 = 'pointtype 9 pointsize 0.5 linecolor rgb "blue"'
a10 = 'pointtype 1 pointsize 0.5 linecolor rgb "magenta"'
a11= 'pointtype 2 pointsize 0.5 linecolor rgb "magenta"'
a12 = 'pointtype 3 pointsize 0.5 linecolor rgb "magenta"'
a13 = 'pointtype 4 pointsize 0.5 linecolor rgb "magenta"'
a14 = 'pointtype 5 pointsize 0.5 linecolor rgb "magenta"'
a15 = 'pointtype 6 pointsize 0.5 linecolor rgb "magenta"'
a16 = 'pointtype 7 pointsize 0.5 linecolor rgb "magenta"'
a17 = 'pointtype 8 pointsize 0.5 linecolor rgb "magenta"'
a18 = 'pointtype 9 pointsize 0.5 linecolor rgb "magenta"'
a19 = 'pointtype 1 pointsize 0.5 linecolor rgb "red"'
a20 = 'pointtype 2 pointsize 0.5 linecolor rgb "red"'
a21= 'pointtype 3 pointsize 0.5 linecolor rgb "red"'
a22 = 'pointtype 4 pointsize 0.5 linecolor rgb "red"'
a23 = 'pointtype 5 pointsize 0.5 linecolor rgb "red"'
a24 = 'pointtype 6 pointsize 0.5 linecolor rgb "red"'
a25 = 'pointtype 7 pointsize 0.5 linecolor rgb "red"'
a26 = 'pointtype 8 pointsize 0.5 linecolor rgb "red"'
a27 = 'pointtype 9 pointsize 0.5 linecolor rgb "red"'
a28 = 'pointtype 1 pointsize 0.5 linecolor rgb "green"'
a29 = 'pointtype 2 pointsize 0.5 linecolor rgb "green"'
a30 = 'pointtype 3 pointsize 0.5 linecolor rgb "green"'
a31 = 'pointtype 4 pointsize 0.5 linecolor rgb "green"'
a32 = 'pointtype 5 pointsize 0.5 linecolor rgb "green"'
a33= 'pointtype 6 pointsize 0.5 linecolor rgb "green"'
a34= 'pointtype 7 pointsize 0.5 linecolor rgb "green"'
a35= 'pointtype 8 pointsize 0.5 linecolor rgb "green"'
a36= 'pointtype 9 pointsize 0.5 linecolor rgb "green"'
a37= 'pointtype 1 pointsize 0.5 linecolor rgb "black"'
a38= 'pointtype 2 pointsize 0.5 linecolor rgb "black"'
a39= 'pointtype 3 pointsize 0.5 linecolor rgb "black"'
a40= 'pointtype 4 pointsize 0.5 linecolor rgb "black"'
a41= 'pointtype 5 pointsize 0.5 linecolor rgb "black"'
set style line 10 lt 2 dashtype 2 lc rgb "black" lw 3
set xrange [:240]
set yrange [0:4.2]
set ytics 1
set key inside top right vertical
set label '(d)'at 15,3.8
set arrow from 0,1. to 240,1. nohead front ls 10
set xtics ("03/21" 7,"05/10" 57,"06/29" 107,"08/18" 157,"10/07" 207)#, "08/12" 257
set title "{/*1.1  National Metropolis}"
plot "metropoles-difRAZAO.dat" u 16:4 title "{/*1.1{Brasília}}" @a26,\
"metropoles-difRAZAO.dat" u 16:10 title "{/*1.1{Rio de Janeiro}}" @a28,\
"metropoles-difRAZAO.dat" u 16:14 title "{/*1.1{São Paulo}}" @a2
