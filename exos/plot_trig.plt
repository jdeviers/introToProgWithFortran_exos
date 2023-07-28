set term pdfcairo enhanced dashed size 3.2,3 font "Calibri Light,14"

set loadpath "/mnt/c/Users/jd756/Documents/gnuplot/palettes/"
load "greens.pal"

set output "trig_test.pdf"
set key Left right top font "Calibri, 10" samplen 2 spacing 1

set ylabel "Angle (deg)"
set ylabel "trig val"

set title "REAL*4"
plot 'real4.dat' u 1:3 w lines ls 3 t "sin(x)",\
			''	 u 1:5 w lines ls 5 t "cos(x)"

set title "REAL*8"
plot 'real8.dat' u 1:3 w lines ls 3 t "sin(x)",\
			''	 u 1:5 w lines ls 5 t "cos(x)"

set title "REAL*16"
plot 'real16.dat' u 1:3 w lines ls 3 t "sin(x)",\
			''	 u 1:5 w lines ls 5 t "cos(x)"