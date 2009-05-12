gnuplot <<EOF
set terminal png transparent nocrop enhanced size 640,480
set log y
set output '$2'
set xlabel 'resemblance'
set ylabel 'frequency'
set title 'resemblance histogram'
plot '$1' notitle with steps
EOF
