#!/usr/bin/env ruby

require 'set'
require 'shingling.rb'
require 'read_data.rb'

FINGERPRINT_SIZE = ARGV.size==0 ? 16 : ARGV[0].to_i
HASH_RANGE = 2**FINGERPRINT_SIZE

class Integer
	def bit_set? idx
		((self >> idx) & 1) == 1
	end
	def bits_differing_with other
		bits = self ^ other
		count = 0
		while(bits!=0) do
			bits &= bits-1
			count += 1
		end
		count
	end
	def rotate
		i = self
		upperbit = i >> FINGERPRINT_SIZE-1 # get upper bit
		i ^= (1 << FINGERPRINT_SIZE-1) if upperbit==1	# toggle upper bit if set
		i <<= 1 # rotate left
		i |= upperbit # append upper bit to lower end
		i
	end
end

class Set
	def fingerprint
		v = [0] * FINGERPRINT_SIZE
		each do |feature|
			hash = feature.hash % HASH_RANGE
			#puts "feature=#{feature}, feature.hash=#{feature.hash}, %HASH_RANGE=#{hash}"
			FINGERPRINT_SIZE.times do |idx|
				bit_set = hash.bit_set? idx		
				v[idx] += bit_set ? 1 : -1
			end
		end
		fingerprint = 0
		FINGERPRINT_SIZE.times do |idx|
			fingerprint |= (1 << idx) if v[idx] > 0
		end
		fingerprint
	end
end

class Fingerprint
	attr_reader :id, :text, :fp
	def initialize id,text
		@id = id
		@text = text
		@fp = text.shingles.fingerprint
	end
	def rotate
		@fp = @fp.rotate
	end
	def print
		printf "%03i %0#{FINGERPRINT_SIZE}b %s\n",@id,@fp,@text
	end
end

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

