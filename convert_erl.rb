#!/usr/bin/env ruby
# convert output from a run of erl sketching
STDIN.each do |line|
	next if line =~ /^stat/;
	next if line =~ /main line/;
	line.sub! 'sic ', ''
  puts line
end

