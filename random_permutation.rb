#!/usr/bin/env ruby

require 'set'
class RandomPermutation

	def initialize max
		@max = max
		@mappings = {}
		@permutations = Set.new
	end

	def permutate value
		raise "value(#{value}) > max(#{max})" if value > @max
		return @mappings[value] if @mappings.has_key? value
		taken = true
		while taken
			new_permutation = rand(@max)
			taken = @permutations.include? new_permutation
		end
		@mappings[value] = new_permutation
		@permutations << new_permutation
		new_permutation
	end

end

=begin
N=30
rp = RandomPermutation.new N
30000.times do 
	n = rand(N)
	puts "#{n}\t#{rp.permutate(n)}\tX"
end
=end
