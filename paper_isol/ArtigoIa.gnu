set term postscript eps enhanced color font 'Helvetica,20'   #  16
set output "SP-din.eps"
set encoding utf8
if (!exists("MP_LEFT"))   MP_LEFT = .15
if (!exists("MP_RIGHT"))  MP_RIGHT = .85
if (!exists("MP_BOTTOM")) MP_BOTTOM = .15
if (!exists("MP_TOP"))    MP_TOP = .9
if (!exists("MP_GAP"))    MP_GAP = 0.1
set multiplot layout 1,1  columnsfirst margins screen MP_LEFT, MP_RIGHT, MP_BOTTOM, MP_TOP spacing screen MP_GAP
set style line 1 linecolor rgb "brown" lw 5 pointtype 7 pointsize 0.8
set style line 2 linecolor rgb "green" lw 5 pointtype 7 pointsize 0.8
set style line 3 linecolor rgb "blue" lw 5 pointtype 7 pointsize 0.8
set style line 4 linecolor rgb "brown" lw 5 pointtype 7 pointsize 0.8
set style line 10 lt 2 dashtype 2 lc rgb "black" lw 3
set object rectangle from 74,0 to 88,3 behind fillcolor rgb 'red' fillstyle solid noborder
set object rectangle from 74,3 to 88,8 behind fillcolor rgb 'orange' fillstyle solid noborder
#set object rectangle from 74,8 to 88,16 behind fillcolor rgb 'orange' fillstyle solid noborder
set object rectangle from 88,0 to 104,16 behind fillcolor rgb 'orange' fillstyle solid noborder
set object rectangle from 104,0 to 160,3 behind fillcolor rgb 'orange' fillstyle solid noborder
set object rectangle from 104,3 to 160,16 behind fillcolor rgb 'yellow' fillstyle solid noborder
set object rectangle from 160,0 to 209,16 behind fillcolor rgb 'yellow' fillstyle solid noborder
set object rectangle from 209,0 to 1000,1000 behind fillcolor rgb 'green' fillstyle solid noborder
set key top right #bottom right
set y2label "{/*1.1{R_t}}
set ylabel "{/*1.1{incidence/100k}}"
set xtics 50
set yrange [0:6.5]
set xrange [0:]
set y2range  [0:3]
set ytics nomirror 2
set y2tics nomirror 1
set arrow from 74,0 to 74,6.5 nohead front dt 3#ls 10
set arrow from 88,0 to 88,6.5 nohead front dt 3#ls 10
set arrow from 97,0 to 97,6.5 nohead front dt 3#ls 10
set arrow from 104,0 to 104,6.5 nohead front dt 3#ls 10
set arrow from 111,0 to 110,6.5 nohead front dt 3#ls 10
set arrow from 118,0 to 118,6.5 nohead front dt 3
set arrow from 125,0 to 125,6.5 nohead front dt 3
set arrow from 132,0 to 132,6.5 nohead front dt 3
set arrow from 139,0 to 139,6.5 nohead front dt 3
set arrow from 146,0 to 146,6.5 nohead front dt 3
set arrow from 160,0 to 160,6.5 nohead front dt 3
set arrow from 174,0 to 174,6.5 nohead front dt 3
set arrow from 181,0 to 181,6.5 nohead front dt 3
set arrow from 209,0 to 209,6.5 nohead front dt 3
set arrow from 224,0 to 224,6.5 nohead front dt 3
set label "{/*1.1 (a)}" at 10,6
set xlabel "{/*1.1{data}}"
set xtics ("03/17" 2,"05/28" 74,"08/08" 146, "10/10" 209)#, "10/09" 209) rotate by 45 right nomirror
set title "{/*1.1 São Paulo}" 
plot "print.dat" u 1:2 axes x1y1 w l ls 1 title '',"print.dat" u 1:3 axes x1y2 w l ls 3 title '',1.0  axes x1y2 dashtype 2 linecolor "black" title ""
