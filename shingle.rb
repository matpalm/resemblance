#!/usr/bin/ruby
require 'set'

class String

	N_GRAM_LEN = 2

	def shingles
		return @cached if @cached	
		n_grams = Set.new
		(length-N_GRAM_LEN+1).times do |i| 
            n_grams << slice(i, N_GRAM_LEN) 
		end	
        @cached = n_grams
		n_grams
	end

	def jaccard_similarity_coeff(b)
		sa = shingles
        sb = b.shingles
		numerator = (sa.intersection sb).size
		denominator = (sa.union sb).size	
		numerator.to_f / denominator  
	end

    def jaccard_distance(b)        
        xor = 0
        union = 0
        shingles.union(b.shingles).each do |shingle|
            in_a = shingles.include? shingle
            in_b = b.shingles.include? shingle
            xor +=1 if in_a ^ in_b
            union +=1 if in_a & in_b
        end
        xor.to_f / (xor+union)
    end

    def invalidate_cache
        @cached = nil
    end

end

raise "shingle.rb [coeff|distance] <min_resemblance=0 for all>" unless ARGV.size==2
TYPE = ARGV[0]
raise "arg0 type can be coeff or distance" unless TYPE=='coeff' or TYPE=='distance'
MIN_RESEMBLANCE = ARGV[1].to_f
WINDOW_SIZE = 100 # following records

MEASURE = TYPE=='coeff' ? 'jaccard_similarity_coeff' : 'jaccard_distance'

# read in
# two lines as file format example...

#5825 Origin Clothing, 38 Sanger St, Corowa, NSW, 2646
#5826 Taburna Pty Ltd, 38 Oxley St, Bourke, NSW, 2840

data = []
STDIN.each do |line|
    line =~ /^([0-9]*) (.*)/;
    id = $1.to_i
    name_addr = $2
	data << [id, name_addr]
end

# process
results =  []
comparisons = 0
(0...data.size).each do |i|
    j_to = i + WINDOW_SIZE
    j_to = data.size if j_to > data.size
    ((i+1)...j_to).each do |j|
        comparisons += 1
        measure = data[i][1].send(MEASURE, data[j][1])
        puts "#{data[i][0]} #{data[j][0]} #{sprintf("%0.05f", measure)}" if measure > MIN_RESEMBLANCE
    end
    data[i][1].invalidate_cache # need to free some memory!!
end
