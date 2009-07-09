#!/usr/bin/env ruby
keys_first = false
(keys_first="false"==ARGV[0]) if ARGV[0]

freqs = {}
freqs.default = 0
STDIN.each do |line|
	line.chomp!
	freqs[line] += 1
end

freqs.keys.each do |k| 
	puts keys_first ? "#{k} #{freqs[k]}" : "#{freqs[k]} #{k}" 
end
