# from introduction to algorithms (cormen, leiserson and rivest) s12.3
# simplified to work with fixed length strings

HASH_SIZE ||= 64

class UniversalHash	
	M = (2**61)-1 # a largish prime
	R = 3
	MAX_VALUE = 2**HASH_SIZE

	def self.build		
		UniversalHash.new((0...R).collect{ (rand() * M).to_i })
	end
	def self.build_with a		
		raise "expected a.size==#{R} to correspond to shingle length" unless a.size==R
		UniversalHash.new a
	end

	def initialize a
		@a = a
	end

	def hash string
		raise "expects strings with length #{R}" unless string.length==3
		value = 0
		string.bytes.each_with_index do |char, index|
			value += (@a[index] * char) % M
		end
		value % MAX_VALUE
	end

end



