require 'shingling.rb'

HASH_SIZE ||= 32
HASH_RANGE = 2**HASH_SIZE

class Integer
	def bit_set? idx
		((self >> idx) & 1) == 1
	end
	def hamming_distance other
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
		upperbit = i >> HASH_SIZE-1 
		i &= ~(1 << HASH_SIZE-1) # unset upper bit
		i <<= 1 # rotate left
		i |= upperbit # append upper bit to lower end
		i
	end
end

class Set
	def simhash hash_fn
		v = [0] * HASH_SIZE
		each do |feature|
			hash = hash_fn.hash(feature) % HASH_RANGE
			#puts "feature=#{feature}, feature.hash=#{feature.hash}, %HASH_RANGE=#{hash}"
			HASH_SIZE.times do |idx|
				bit_set = hash.bit_set? idx		
				v[idx] += bit_set ? 1 : -1
			end
		end
		simhash = 0
		HASH_SIZE.times do |idx|
			simhash |= (1 << idx) if v[idx] > 0
		end
		simhash
	end
end

class SimHash
	attr_reader :id, :text, :simhash
	def initialize id,text,hash_fn
		@id = id
		@text = text
		@simhash = text.shingles.simhash hash_fn
	end
	def rotate
		@simhash = @simhash.rotate
	end
	def print
		printf "%03i %0#{HASH_SIZE}b %i %s\n",@id,@simhash,@simhash,@text
	end
end

