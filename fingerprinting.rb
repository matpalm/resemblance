require 'shingling.rb'

FINGERPRINT_SIZE ||= 32
HASH_RANGE = 2**FINGERPRINT_SIZE

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
		upperbit = i >> FINGERPRINT_SIZE-1 
		i &= ~(1 << FINGERPRINT_SIZE-1) # unset upper bit
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
		printf "%03i %0#{FINGERPRINT_SIZE}b %i %s\n",@id,@fp,@fp,@text
	end
end

