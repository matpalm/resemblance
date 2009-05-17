#!/usr/bin/env ruby

require 'set'
require 'read_data.rb'
require 'util.rb'

WINDOW_SIZE = 10
NUM_THREADS = 4
RAM_DIR = "/dev/shm"

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

simhashs = data.collect do |item|
	index, phrase = item
	simhash = SimHash.new(index, phrase.chomp)
end

simhashs_by_id = {}
simhashs.each { |sh| simhashs_by_id[sh.id] = sh }

NUM_THREADS.times do |i|
	fork do
		candidates = Set.new

		10.times do
			universal_hashfn = UniversalHash.build #_with [10,20,30]
			simhashs.each { |sh| sh.apply_hash_fn universal_hashfn }

			#puts "unsorted"
			#simhashs.each { |simhash| simhash.print }

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
			#	if i!=HASH_SIZE-1
			#		simhashs.each { |sh| sh.rotate }
			#	end
		end

		file = File.new("#{RAM_DIR}/dump.#{i}",'w')
		file.write Marshal.dump(candidates)
		file.close

	end
end
NUM_THREADS.times { Process.wait }

candidates = Set.new
NUM_THREADS.times do |i|
	file = File.open("#{RAM_DIR}/dump.#{i}",'r')
  candidates += Marshal.load file.read
	file.close		
end

candidates = candidates.to_a
#candidates = candidates.sort_by {|c| c[1] }.reverse

candidates.each do |c|
	phrase1 = simhashs_by_id[c[0]].text
	phrase2 = simhashs_by_id[c[1]].text
	coeff = phrase1.jaccard_similarity_coeff phrase2
	printf "%i %i %0.05f\n", c[0], c[1], coeff if coeff >= MIN_RESEMBLANCE
end

