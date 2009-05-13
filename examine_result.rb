#!/usr/bin/env ruby

# collate a results file with the phrases the results represent
#
# eg stdin of the form
#  1 2 0.4
#  2 3 0.6 
#
# and a reference file of the form
#  1 ice
#  2 cream
#  3 van
#
# and output
#  0.4
#   ice
#   cream
#  0.6
#   cream
#   van

raise "usage: cat some_resemblance_measure.result | ./examine_result.rb phrases" unless ARGV.size == 1

require 'read_data'
RESULT_FILE = ARGV.first
data = read_data File.open(RESULT_FILE)

STDIN.each do |line|
	a,b,coeff = line.chomp.split
	a,b = a.to_i, b.to_i
	puts "#{coeff} (#{a},#{b})"
	puts "\t#{data[a]}"
	puts "\t#{data[b]}"
end


