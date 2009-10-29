#!/usr/bin/env ruby
raise "usage: filter_over.rb N" unless ARGV.length==1 
min_freq = ARGV[0].to_i
STDIN.each do |line|
	line =~ /(\s+\d+) /
	freq = $1
	puts line if freq.to_i >= min_freq
end
