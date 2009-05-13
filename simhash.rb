#!/usr/bin/env ruby

require 'set'
require 'read_data.rb'

FINGERPRINT_SIZE = ARGV.size==0 ? 16 : ARGV[0].to_i
require 'fingerprinting.rb'

data = read_data

fps_by_id = {}
fps = []
data.each do |item|
	index, phrase = item
	fp = Fingerprint.new(index, phrase.chomp)
	fps_by_id[index] = fp
	fps << fp
end

#puts "unsorted"
#fps.each { |fp| fp.print }

candidates = Set.new
FINGERPRINT_SIZE.times do |i|
	#puts "sorted #{i}"
	fps = fps.sort_by { |v| v.fp }
	#fps.each { |f| f.print }

	last_fp = nil
	fps.each do |fp|
		if last_fp
			c1,c2 = fp.id, last_fp.id
			c1,c2 = c2,c1 if c1 > c2
			candidate = [c1,c2]
			candidates << candidate
		end
#		fp.print
		last_fp = fp
	end

	# rotate each, except last time
	if i!=FINGERPRINT_SIZE-1
		fps.each { |fp| fp.rotate }
	end
end

candidates = candidates.to_a
candidates = candidates.sort_by {|c| c[1] }.reverse
candidates.each do |c|
	phrase1 = fps_by_id[c[0]].text
	phrase2 = fps_by_id[c[1]].text
	coeff = phrase1.jaccard_similarity_coeff phrase2
	printf "%i %i %0.05f\n", c[0], c[1], coeff
end

