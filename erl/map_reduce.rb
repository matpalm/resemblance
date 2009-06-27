#!/usr/bin/env ruby

@cmd = 0

def append_to_log msg
 `echo #{msg} >> stats.out`
end

def run(command)
	@cmd += 1
  append_to_log "cmd #{@cmd}"
	start = Time.now
	`#{command} > #{@cmd}.out`
	now = Time.now
	puts "#{@cmd}\t#{now-start} secs\t#{command} > #{@cmd}.out"
end

NUM = ARGV[0] || "10"
FILES = ARGV[1] || "10"

msg = "NUM=#{NUM} FILES=#{FILES}"
append_to_log msg
puts msg

run "rm -rf mr*_* *vm *out"
run "head -n #{NUM} ../name_addr | erl -noshell -pa ebin -s prepare -num_files #{FILES} -output_dir mr01_prepared"

# for each line of input emit sketch values for each shingle { SketchValue, DocId }
run "erl -noshell -pa ebin -s map_reduce_s -task sketch_mapper -input_dir mr01_prepared -output_dir mr02_mapped"

# shuffle on sketch value -> { SketchValue, [DocId, DocId, ... ] }
run "erl -noshell -pa ebin -s map_reduce -task partition -num_partitions #{FILES} -input_dir mr02_mapped -output_dir mr03_partitioned"
run "erl -noshell -pa ebin -s map_reduce_s -task sort_collate -input_dir mr03_partitioned -output_dir mr04_shuffled_seperate"
run "erl -noshell -pa ebin -s merge -input_dir mr04_shuffled_seperate -output_dir mr05_shuffled" 

# emit all combos; { 123, [1,2,3] } emits {[1,2],1} {[1,3],1} {[2,3],1} 
run "erl -noshell -pa ebin -s map_reduce_s -task reducer -input_dir mr05_shuffled -output_dir mr06_reduced"

# shufle on doc id pairs { DocIdPair, [1,1,1, ...  ] }
run "erl -noshell -pa ebin -s map_reduce -task partition -num_partitions #{FILES} -input_dir mr06_reduced -output_dir mr07_partitioned2"
run "erl -noshell -pa ebin -s map_reduce_s -task sort_collate -input_dir mr07_partitioned2 -output_dir mr08_shuffled_seperate2"
run "erl -noshell -pa ebin -s merge -input_dir mr08_shuffled_seperate2 -output_dir mr09_shuffled2"

# sum docid pair freqs, 
# emits freq first, not doc id pair 
# only emits if freq > 5
#{ DocIdPair, [1,1,1] } -> no emit
#{ DocIdPair, [1,1,1,1,1,1] } -> emit { 6, DocIdPair }
run "erl -noshell -pa ebin -s map_reduce_s -task sum_reducer -min_sum 8 -input_dir mr09_shuffled2 -output_dir mr10_reduced2"

# final output
`./scat mr10_reduced2/* | perl -plne's/.*\{(.*?),(.*?)\}.*/$1 $2/;' > pairs.#{NUM}`

puts `du -sh mr*_*`

# collect doc id pairs with same frequency, needs a filter as well?
#time erl -noshell -pa ebin -s partition -input_files mr_reduced2/* -output_dir mr_partitioned3
#time erl -noshell -pa ebin -s map_reduce main shuffle -input_files mr_partitioned3/* -output_dir mr_shuffled3

