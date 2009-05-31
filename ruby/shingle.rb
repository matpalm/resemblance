#!/usr/bin/ruby
require 'set'
require File.dirname(__FILE__)+'/shingling.rb'
require File.dirname(__FILE__)+'/read_data.rb'

raise "shingle.rb [coeff|distance] <min_resemblance=0 for all>" unless ARGV.size==2
TYPE = ARGV[0]
raise "arg0 type can be coeff or distance" unless TYPE=='coeff' or TYPE=='distance'
MIN_RESEMBLANCE = ARGV[1].to_f
WINDOW_SIZE = 100 # following records

MEASURE = TYPE=='coeff' ? 'jaccard_similarity_coeff' : 'jaccard_distance'

data = read_data.to_a.sort_by {|d| d[0]}

# process
results =  []
comparisons = 0
(0...data.size).each do |i|
	j_to = data.size# i + WINDOW_SIZE
	j_to = data.size if j_to > data.size
	((i+1)...j_to).each do |j|
		comparisons += 1
		measure = data[i][1].send(MEASURE, data[j][1])
		printf "%i %i %0.05f\n", data[i][0], data[j][0], measure if measure >= MIN_RESEMBLANCE
	end
	data[i][1].invalidate_cache # need to free some memory!!
end
#STDERR.puts "comparisons=#{comparisons}"

