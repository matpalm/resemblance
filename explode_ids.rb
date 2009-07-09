#!/usr/bin/env ruby

# explode a list of ids; 
# from 
#  1 2 3
# to 
#  1 2
#  1 3
#  2 3
STDIN.each do |line|
	ids = line.split
	next if ids.length == 2 # HACK
	while not ids.empty? 
		id1 = ids.shift
		ids.each { |id2| puts "#{id1} #{id2}" }
	end
end
