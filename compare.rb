#!/usr/bin/env ruby
# compare simhash and shingle algorithms by taking top N from shingling and seeing
# how many the simhash algo got right

raise "check args" unless ARGV.length==0 or ARGV.length==3
RESEMBLANCE_CUTOFF = ARGV[0].to_f || 0
SHINGLING_OUT_FILE = ARGV[1] || 'shingling.result'
SIMHASH_OUT_FILE = ARGV[2] || 'simhash.result'

require 'set'
top_from_shingling = Set.new
File.open(SHINGLING_OUT_FILE).each do |line|
	a,b,coeff = line.chomp.split
	break if coeff.to_f < RESEMBLANCE_CUTOFF
	top_from_shingling << [a,b]	
end

total = hits = 0
File.open(SIMHASH_OUT_FILE).each do |line|
	total += 1
	a,b,coeff = line.chomp.split
	hits += 1 if top_from_shingling.include? [a,b]
	break if total == top_from_shingling.size
end

puts "simhash got #{hits} out of a possible #{total} (#{hits.to_f/total*100}%) values above #{RESEMBLANCE_CUTOFF}" 
