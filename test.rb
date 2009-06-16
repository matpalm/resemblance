#!/usr/bin/env ruby
# arg1 = number of entries to take from test data

USE_RUBY = false

raise "usage: test.rb NUM_ENTRIES MIN_RESEMBLANCE=0.6" unless ARGV.size==1 or ARGV.size==2
N = ARGV[0].to_i
MIN_RES = ARGV[1] ? ARGV[1].to_f : 0.6
puts "NUM_ENTRIES=#{N} MIN_RESEMBLANCE=#{MIN_RES}"

raise "test.rb configured to assume min_resem >= 0.6" unless MIN_RES >= 0.6 and MIN_RES <= 1

alias _puts puts 
def puts msg
	_puts "#{Time.now} #{msg}"
end

def time_call
	start = Time.now
	yield
	puts "time #{Time.now-start}s" 
end

def run_unless_file_exists file
	if File.exists? file
		puts "using cached version of #{file}"	
	else
		puts "calculating #{file}"
		yield
	end
end

def head_command n
#	"head -n #{n} name_addr | perl -pnle'tr/A-Z/a-z/'"
	"head -n #{n} name_addr"
end

def run command
	puts "exec #{command}"
	`#{command}`
end

def combine_resemblance_results file
	puts "combining resemblance.out files"
	resems = Dir.glob("resemblance.*.out")
	resems_sorted = resems.collect {|r| "#{r}.sorted"}
	resems.zip(resems_sorted).each { |p| `sort -rn -k3 < #{p[0]} > #{p[1]}` }
	`sort -rnm -k3 #{resems_sorted.join(' ')} > #{file}`
	resems.each { |r| File.delete(r) }
	resems_sorted.each { |r| File.delete(r) }
end

puts "---running #{N} entries"

# assuming never want to compare any resemblances UNDER 0.6 so 
# its hardcoded. strange things happen if a run is done with x and then y using
# cached x when y < x

puts "---running shingling (#{USE_RUBY ? "ruby" : "cpp"} version)"
file = "shingle.result.#{N}"
run_unless_file_exists(file) do
	if USE_RUBY
		time_call { run "#{head_command(N)} | ruby shingle.rb coeff 0.6 > #{file}" }
		`cat #{file} | sort -rn -k3 > tmp.#{$$}`
		`mv tmp.#{$$} #{file}`
	else
		time_call { run "#{head_command(N)} | ./cpp/bin/Release/resemblance 0.6" }
		combine_resemblance_results file		
	end
end
puts "#lines= #{`wc -l shingle.result.#{N}`}"

=begin
NUM_BITS = 64
puts "---running simhash #{NUM_BITS} bit"
file = "simhash.result.#{N}.#{NUM_BITS}"
run_unless_file_exists(file) do
	time_call { run "head -n #{N} name_addr | ruby simhash.rb #{NUM_BITS} 0.6" }
	combine_resemblance_results file 	
end
puts "#lines= #{`wc -l #{file}`}"
puts run "./compare.rb  shingle.result.#{N} #{file} #{MIN_RES}"
=end

def compare_sketch num_bits, sketch_size, cutoff
	puts "---running sketch num_bits=#{num_bits} sketch_size=#{sketch_size} cutoff=#{cutoff}"
	file = "sketch.result.#{N}.#{num_bits}b.#{sketch_size}s.#{cutoff}c"
	run_unless_file_exists(file) do
		time_call { run "#{head_command(N)} | ruby ruby/sketch.rb #{num_bits} #{sketch_size} #{cutoff} > #{file}" }
	end
	puts "#lines= #{`wc -l #{file}`}"
	puts run "./compare.rb shingle.result.#{N} #{file} #{MIN_RES}"
end

def compare_erl_sketch 
	puts "---running erl sketch"
	file = "sketch.result.erl.#{N}"
	run_unless_file_exists(file) do	
		run "head -n #{N} name_addr > test.data"
		time_call { run "erl -noshell -pa erl/ebin -s main main  > #{file}" }
	end
	puts "#lines= #{`wc -l #{file}`}"
	puts run "./compare.rb shingle.result.#{N} #{file} #{MIN_RES}"
end

def compare_erl_sketch2
	puts "---running erl sketch"
	file = "sketch.result.erl.#{N}"
	run_unless_file_exists(file) do	
		run "head -n #{N} name_addr > test.data"
		tmp_file = "/tmp/erl.#{$$}.out"
		time_call do 
			run "cat test.data | erl -noshell -pa erl/ebin -s main main | ./convert_erl.rb | ./freq.rb false > #{tmp_file}"
			run "cat test.data | ./determine_jaccard.rb #{tmp_file} > #{file}"
		end
#		run "rm #{tmp_file}"
	end
	puts "#lines= #{`wc -l #{file}`}"
	puts run "./compare.rb shingle.result.#{N} #{file} #{MIN_RES}"
end

def compare_erl_map_reduce
	puts "---running erl sketch"
	file = "sketch.result.erl.mr.#{N}"	
	run_unless_file_exists(file) do	
		run "head -n #{N} name_addr > test.data"
		run "mkdir sics_map 2>/dev/null"
		run "rm sics_map/*"
		time_call do 
			run "cat test.data | erl -noshell -pa erl/ebin -s map main > map.out"
			run "erl -noshell -pa erl/ebin -s reduce main -files sics_map/* > #{file}"
		end
	end
	puts "#lines= #{`wc -l #{file}`}"
	puts run "./compare.rb shingle.result.#{N} #{file} #{MIN_RES}"
end

compare_erl_map_reduce

#compare_sketch 64, 10, 2
#compare_sketch 64, 10, 2
#compare_erl_sketch2

