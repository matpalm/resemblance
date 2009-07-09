#!/usr/bin/env ruby
CUTOFF = (ARGV[0] || "0.6").to_f
STDIN.each do |line|
	id1,id2,res = line.chomp.split
	puts line if res.to_f > 0.6
end
