#!/usr/bin/env ruby

postcode_to_line = {}
STDIN.each do |line|
	cols = line.split '|'
	postcode = cols[5].to_i
	
	old_format = cols[0] + ' '
	old_format += [1,2].collect{|i| cols[i]}.join(',')

	postcode_to_line[postcode] ||= []
	postcode_to_line[postcode] << old_format
end

ROOT = `pwd`.chomp
SPLIT_DIR = "#{ROOT}/split_per_postcode"
`mkdir #{SPLIT_DIR} 2>/dev/null`
postcode_to_line.each do |postcode, lines|
	out = File.open "#{SPLIT_DIR}/#{postcode}", 'w'
	lines.each {|l| out.puts l}
	out.close
end

RESEM_DIR = "#{ROOT}/resem_per_postcode"
`mkdir #{RESEM_DIR} 2>/dev/null`
`ls #{SPLIT_DIR}`.each do |postcode_file|	
	postcode_file.chomp!
	command = "cd /dev/shm && cat #{SPLIT_DIR}/#{postcode_file} | #{ROOT}/cpp/bin/Release/resemblance 0.5 && cat resemblance*out > #{RESEM_DIR}/#{postcode_file}"
 	puts command
	`#{command}`
end

