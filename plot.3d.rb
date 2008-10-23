#!/usr/bin/env ruby
puts <<EOF
set terminal png transparent nocrop enhanced size 480,360
set xrange [-0.1:] 
set yrange [-0.1:]
EOF

angle = 0
while angle < 720
	frame = sprintf("%03d",(angle / 5))	
	puts "set output '3dplot_#{frame}.png'"
	puts "set view 60,#{angle%360}"
	puts "splot 'points.3d' with labels notitle"	
	angle += 5
end

