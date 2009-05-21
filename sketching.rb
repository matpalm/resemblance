require 'universal_hash'
require 'random_permutation'

class Sketch

	def initialize elems
		@elems = elems
		@sketch = []
	end

	def apply_hash hash_fn
		@hashes = @elems.each.collect { |elem| hash_fn.hash elem }
	end

	def apply_permutation permutation
		@hashes = @hashes.collect { |h| permutation.permutate h }
		sketch << @hashes.min
	end

	def sketch
		@sketch
	end

end

class Sketches
	
	def self.calculate_for elems
		sketches = elems.collect { |e| Sketch.new(e) }

		hash_fn = UniversalHash.build
		sketches.each { |s| s.apply_hash hash_fn }

		SKETCH_SIZE.times do 
			permutation = RandomPermutation.new hash_fn.max_value		
			sketches.each { |s| s.apply_permutation permutation }
		end
		
		sketches
	end

end
	
