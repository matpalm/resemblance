#!/usr/bin/env ruby

# collate a results file with the phrases the results represent
#
# eg stdin with reference info of the form
#  1 ice
#  2 cream
#  4 van
#
# and a results file
#  1 2 0.4
#  2 4 0.6 
#
# and output
#  0.4
#   ice
#   cream
#  0.6
#   cream
#   van

raise "usage: head -n 500 phrases | ./examine_result.rb some_resemblance_measure.result.500" unless ARGV.size == 1

require 'ruby/read_data'
data = read_data STDIN

File.open(ARGV.first).each do |line|
	a,b,coeff = line.chomp.split
	a,b = a.to_i, b.to_i
	puts "#{coeff} (#{a},#{b})"
	puts "\t#{data[a]}"
	puts "\t#{data[b]}"
end


