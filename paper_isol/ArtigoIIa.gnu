set term postscript eps enhanced color font 'Helvetica,20'   #  16
set output "SP-taxa.eps"
set encoding utf8
if (!exists("MP_LEFT"))   MP_LEFT = .15
if (!exists("MP_RIGHT"))  MP_RIGHT = .85
if (!exists("MP_BOTTOM")) MP_BOTTOM = .15
if (!exists("MP_TOP"))    MP_TOP = .9
if (!exists("MP_GAP"))    MP_GAP = 0.1
set multiplot layout 1,1  columnsfirst margins screen MP_LEFT, MP_RIGHT, MP_BOTTOM, MP_TOP spacing screen MP_GAP
set object rectangle from 53,0 to 70,1000 behind fillcolor rgb 'gray60' fillstyle solid noborder
set object rectangle from 3,0 to 6,1000 behind fillcolor rgb 'gray20' fillstyle solid noborder
set object rectangle from 77,0 to 80,1000 behind fillcolor rgb 'gray90' fillstyle solid noborder
set style line 1 linecolor rgb "blue" lw 3 pointtype 7 pointsize 1.0
set style line 2 linecolor rgb "green" lw 3 pointtype 7 pointsize 0.5
set style line 3 linecolor rgb "blue" lw 3 pointtype 7 pointsize 0.8
set style line 4 linecolor rgb "brown" lw 3 pointtype 7 pointsize 1.0
set style line 10 lt 2 dashtype 2 lc rgb "black" lw 3
set key top right #bottom right
set pm3d map
set cbtics 50 #("03/16" 2,"05/27" 74,"08/07" 146, "10/09" 209)
set ylabel "{/*1.1{transmission index}}
set xlabel "{/*1.1{isolation index}}
set label"{/*1.1{(b)}" at 0.27,0.33
set title "{/*1.1 São Paulo}"
#plot [0.01:0.32][0.03:0.5] "print.dat" u 4:(($1<=73)? $5:1/0) w p lw 6 lc rgb 'black' title '', "print.dat" u 4:(($1>73 && $1<=88)? $5:1/0) w p lw 6 lc rgb 'red' title '',"print.dat" u 4:(($1>88 && $1<=160)? $5:1/0) w p lw 6 lc rgb 'orange' title '',"print.dat" u 4:(($1>160 && $1<=209)? $5:1/0) w p lw 6 lc rgb 'yellow' title '',"print.dat" u 4:(($1>209? $5:1/0)) w p lw 6 lc rgb 'green' title ''
plot [0.01:0.3][0.03:0.36] "printLag.dat" u 4:(($1<=73)? $5:1/0) w p lw 6 lc rgb 'black' title '', "printLag.dat" u 4:(($1>73 && $1<=88)? $5:1/0) w p lw 6 lc rgb 'red' title '',"printLag.dat" u 4:(($1>88 && $1<=160)? $5:1/0) w p lw 6 lc rgb 'orange' title '',"printLag.dat" u 4:(($1>160 && $1<=209)? $5:1/0) w p lw 6 lc rgb 'yellow' title '',"printLag.dat" u 4:(($1>209? $5:1/0)) w p lw 6 lc rgb 'green' title ''

#
