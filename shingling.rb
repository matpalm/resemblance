require 'set'

N_GRAM_LEN = 3

class String

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


