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

phrases = read_data
simhashs = phrases.collect do |id_text|
	index, phrase = id_text
	simhash = SimHash.new(index, phrase.chomp)
end


NUM_THREADS.times do |i|
	fork do
		candidates = Set.new

		universal_hashfn = UniversalHash.build #_with [10,20,30]
		simhashs.each { |sh| sh.apply_hash_fn universal_hashfn }

		HASH_SIZE.times do |i|
			#puts "iter #{i}"

			#puts "unsorted"
			#simhashs.each { |simhash| simhash.print }

			simhashs = simhashs.sort_by { |sh| sh.simhash }
			#simhashs.each { |h| h.print }

			(0...simhashs.size).each do |i|
				sh1 = simhashs[i]
				c1 = sh1.id 

				j_to = i + WINDOW_SIZE
				j_to = simhashs.size if j_to > simhashs.size
				((i+1)...j_to).each do |j|
					sh2 = simhashs[j]			
					c2 = sh2.id					
					c1,c2 = c2,c1 if c1 > c2
	#				puts 	"i #{i} c1 #{c1}"+
	#							" j #{j} c2 #{c2}"+
	#							" dist #{simhashs[i].simhash.hamming_distance(simhashs[j].simhash)}"+
	#							" jaccard #{phrases[c1].jaccard_similarity_coeff phrases[c2]}" 
					candidates << [c1,c2]
				end
			end

			if i!=HASH_SIZE-1
				simhashs.each { |sh| sh.rotate }
			end
		end

		file = File.new("#{RAM_DIR}/dump.#{i}",'w')
		file.write Marshal.dump(candidates)
		file.close

	end
end
NUM_THREADS.times { Process.wait }

# todo this takes _minutes_ how can it be optimised? set.to_a and merge sort?
candidates = Set.new
NUM_THREADS.times do |i|
	next_set = Marshal.load(IO.read("#{RAM_DIR}/dump.#{i}"))
	candidates += next_set
end

candidates_chunks = candidates.to_a.chunks(NUM_THREADS)
candidates_chunks.each_with_index do |candidates, idx|
	fork do
		out = File.new("resemblance.#{idx}.out",'w')
		candidates.each do |c|
			c1, c2 = c
			coeff = phrases[c1].jaccard_similarity_coeff phrases[c2]
			out.printf "%i %i %0.05f\n", c[0], c[1], coeff if coeff >= MIN_RESEMBLANCE
		end
		out.close
	end
end
candidates_chunks.size.times { Process.wait }

