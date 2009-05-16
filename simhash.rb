#!/usr/bin/env ruby

puts "start"
require 'set'
require 'read_data.rb'

WINDOW_SIZE = 10

raise "usage: simhash.rb SIMHASH_SIZE=32 MIN_RESEMBLANCE=0" unless ARGV.size==0 or ARGV.size==2
SIMHASH_SIZE = ARGV.size==0 ? 32 : ARGV[0].to_i
MIN_RESEMBLANCE = ARGV.size==0 ? 0 : ARGV[1].to_f
require 'simhashing.rb'

alias _puts puts
def puts msg
	_puts "#{Time.now} #{msg}"
end

puts ">read_data"
data = read_data
puts "<read_data"

simhashs_by_id = {}
simhashs = []
puts ">simhashing"
i =0
data.each do |item|
	i+=1
	puts "simhashing #{i} / #{data.size}" if i%1000==0
	index, phrase = item
	simhash = SimHash.new(index, phrase.chomp)
	simhashs_by_id[index] = simhash
	simhashs << simhash
end
puts "<simhashing"

#puts "unsorted"
#simhashs.each { |simhash| simhash.print }

candidates = Set.new
SIMHASH_SIZE.times do |i|
	puts ">sorting #{i}"
	simhashs = simhashs.sort_by { |sh| sh.simhash }
	#simhashs.each { |h| h.print }
	puts "<sorting #{i}"

	puts ">building candidates"
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
	puts "< building candidates (candidates.size=#{candidates.size})"

	# rotate each, except last time
	puts ">rotating"
	if i!=SIMHASH_SIZE-1
		simhashs.each { |sh| sh.rotate }
	end
	puts "<rotating"
end

candidates = candidates.to_a
candidates = candidates.sort_by {|c| c[1] }.reverse

puts ">eval similarity"
candidates.each do |c|
	phrase1 = simhashs_by_id[c[0]].text
	phrase2 = simhashs_by_id[c[1]].text
	coeff = phrase1.jaccard_similarity_coeff phrase2
	printf "%i %i %0.05f\n", c[0], c[1], coeff if coeff >= MIN_RESEMBLANCE
end
puts "<eval similarity"
