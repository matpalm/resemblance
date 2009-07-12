#!/usr/bin/env ruby
STDIN.each do |line|
	puts line.split('|')[0,6].join('|')
end

