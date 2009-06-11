#!/usr/bin/env ruby

# determine jaccard coeff for a number of doc id pairs

raise "usage: head -n 500 documents | ./determine_jaccard.rb some_resemblance_measure.result.500" unless ARGV.size == 1

require File.dirname(__FILE__)+"/ruby/read_data"
require File.dirname(__FILE__)+"/ruby/shingling"

data = read_data STDIN

File.open(ARGV.first).each do |line|
	a,b = line.chomp.split.map{|n| n.to_i}
	printf "%i %i %0.05f\n", a, b, data[a].jaccard_similarity_coeff(data[b])
end


