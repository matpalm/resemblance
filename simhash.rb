#!/usr/bin/env ruby

require 'set'
require 'read_data.rb'

SIMHASH_SIZE = ARGV.size==0 ? 16 : ARGV[0].to_i
require 'simhashing.rb'

data = read_data

simhashs_by_id = {}
simhashs = []
data.each do |item|
	index, phrase = item
	simhash = SimHash.new(index, phrase.chomp)
	simhashs_by_id[index] = simhash
	simhashs << simhash
end

#puts "unsorted"
#simhashs.each { |simhash| simhash.print }

candidates = Set.new
SIMHASH_SIZE.times do |i|
	#puts "sorted #{i}"
	simhashs = simhashs.sort_by { |sh| sh.simhash }
	#simhashs.each { |h| h.print }

	last_sh = nil
	simhashs.each do |sh|
		if last_sh
			c1,c2 = sh.id, last_sh.id
			c1,c2 = c2,c1 if c1 > c2
			candidate = [c1,c2]
			candidates << candidate
		end
#		simhash.print
		last_sh = sh
	end

	# rotate each, except last time
	if i!=SIMHASH_SIZE-1
		simhashs.each { |sh| sh.rotate }
	end
end

candidates = candidates.to_a
candidates = candidates.sort_by {|c| c[1] }.reverse
candidates.each do |c|
	phrase1 = simhashs_by_id[c[0]].text
	phrase2 = simhashs_by_id[c[1]].text
	coeff = phrase1.jaccard_similarity_coeff phrase2
	printf "%i %i %0.05f\n", c[0], c[1], coeff
end

