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

hits = 0
File.open(SIMHASH_OUT_FILE).each do |line|
	a,b,coeff = line.chomp.split
	hits += 1 if top_from_shingling.include? [a,b]
end

printf "simhash got %i out of a possible %i (%i%%) values above %s\n", hits, top_from_shingling.size, (hits.to_f/top_from_shingling.size*100), RESEMBLANCE_CUTOFF
