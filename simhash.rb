#!/usr/bin/env ruby

require 'set'
require 'read_data.rb'

WINDOW_SIZE = 10

raise "usage: simhash.rb SIMHASH_SIZE=32 MIN_RESEMBLANCE=0" unless ARGV.size==0 or ARGV.size==2
HASH_SIZE = ARGV.size==0 ? 32 : ARGV[0].to_i
MIN_RESEMBLANCE = ARGV.size==0 ? 0 : ARGV[1].to_f

require 'simhashing.rb'
require 'universal_hash.rb'

alias _puts puts
def puts msg
	_puts "#{Time.now} #{msg}"
end

data = read_data

universal_hashfn = UniversalHash.build #_with [10,20,30]
simhashs_by_id = {}
simhashs = []
i =0
data.each do |item|
	i+=1
#	puts "simhashing #{i} / #{data.size}" if i%1000==0
	index, phrase = item
	simhash = SimHash.new(index, phrase.chomp, universal_hashfn)
	simhashs_by_id[index] = simhash
	simhashs << simhash
end

#puts "unsorted"
#simhashs.each { |simhash| simhash.print }

candidates = Set.new
HASH_SIZE.times do |i|
	#puts "iter #{i}"

	simhashs = simhashs.sort_by { |sh| sh.simhash }
	#simhashs.each { |h| h.print }

	(0...simhashs.size).each do |i|
		j_to = i + WINDOW_SIZE
		j_to = simhashs.size if j_to > simhashs.size
		((i+1)...j_to).each do |j|
			c1,c2 = simhashs[i].id, simhashs[j].id
			#puts "i=#{i} j=#{j} c1=#{c1} c2=#{c2}"
			c1,c2 = c2,c1 if c1 > c2
			candidates << [c1,c2]
		end
	end

	# rotate each, except last time
	if i!=HASH_SIZE-1
		simhashs.each { |sh| sh.rotate }
	end
end

candidates = candidates.to_a
candidates = candidates.sort_by {|c| c[1] }.reverse

candidates.each do |c|
	phrase1 = simhashs_by_id[c[0]].text
	phrase2 = simhashs_by_id[c[1]].text
	coeff = phrase1.jaccard_similarity_coeff phrase2
	printf "%i %i %0.05f\n", c[0], c[1], coeff if coeff >= MIN_RESEMBLANCE
end

