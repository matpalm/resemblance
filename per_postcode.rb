#!/usr/bin/env ruby

raise "usage: per_postcode.rb NUM_LINES" unless ARGV.length==1
LINES = ARGV.shift

NUM_CPUS = 4

def run command
	puts command
	`#{command}`
end

# split per postcode
run "rm -rf resem_per_postcode split_per_postcode result_per_postcode cc_image_per_postcode"
run "mkdir resem_per_postcode split_per_postcode result_per_postcode cc_image_per_postcode"
run "head -n #{LINES} name_addr | ./sans_phone_heading.rb | sort -k2 -t\\| | ./find_dups.rb test"

# run cpp shingler per postcode
run "cat split/test.unique | ./cpp_resem_per_postcode.rb"

# human readable 
run "cat resem_per_postcode/* > resem_per_postcode.all"
run "cat split/test.unique | ./examine_result.rb resem_per_postcode.all > examined_result" 

# connected components
@postcodes = `ls resem_per_postcode`.collect{|l| l.chomp}
def fork_for_next_postcode
	postcode = @postcodes.shift
	fork do
 		run "cat resem_per_postcode/#{postcode} | ./connected_components.rb > result_per_postcode/#{postcode}" 
#		run "cat resem_per_postcode/#{postcode} | ./result_to_dot.rb cc_image_per_postcode/#{postcode}.png"
	end
end
NUM_CPUS.times { fork_for_next_postcode }
while not @postcodes.empty?
	Process.wait
	fork_for_next_postcode
end
NUM_CPUS.times { Process.wait }

