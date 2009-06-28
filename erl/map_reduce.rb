#!/usr/bin/env ruby

@cmd = 0
@last = Time.now

def log msg
	`echo #{msg} >> stats.out`
end

def run(command)
	@cmd += 1
	now = Time.now
	puts "#{now} (#{now-@last}sec) #{@cmd} #{command}"
  log "S #{@cmd}"
	`#{command} > #{@cmd}.out`
	log "E #{@cmd} DU #{`du -sh mr`.chomp}"
	@last = now
end

NUM = ARGV[0] || "10"
FILES = ARGV[1] || "10"

msg = "NUM=#{NUM} FILES=#{FILES}"
log msg

run "head -n #{NUM} ../name_addr | erl -noshell -pa ebin -s prepare -num_files #{FILES} -output_dir mr/01_prepared"

# for each line of input emit sketch values for each shingle { SketchValue, DocId }
run "erl -noshell -pa ebin -s map_reduce_s -task sketch_mapper -input_dir mr/01_prepared -output_dir mr/02_mapped"

# shuffle on sketch value -> { SketchValue, [DocId, DocId, ... ] }
run "erl -noshell -pa ebin -s map_reduce -task partition -num_partitions #{FILES} -input_dir mr/02_mapped -output_dir mr/03_partitioned"
run "erl -noshell -pa ebin -s map_reduce_s -task sort_collate -input_dir mr/03_partitioned -output_dir mr/04_shuffled_seperate"
run "erl -noshell -pa ebin -s merge -input_dir mr/04_shuffled_seperate -output_dir mr/05_shuffled" 

# emit all combos; { 123, [1,2,3] } emits {[1,2],1} {[1,3],1} {[2,3],1} 
run "erl -noshell -pa ebin -s map_reduce_s -task reducer -input_dir mr/05_shuffled -output_dir mr/06_reduced"

# shufle on doc id pairs { DocIdPair, [1,1,1, ...  ] }
run "erl -noshell -pa ebin -s map_reduce -task partition -num_partitions #{FILES} -input_dir mr/06_reduced -output_dir mr/07_partitioned2"
run "erl -noshell -pa ebin -s map_reduce_s -task sort_collate -input_dir mr/07_partitioned2 -output_dir mr/08_shuffled_seperate2"
run "erl -noshell -pa ebin -s merge -input_dir mr/08_shuffled_seperate2 -output_dir mr/09_shuffled2"

# sum docid pair freqs, 
# emits freq first, not doc id pair 
# only emits if freq > 5
#{ DocIdPair, [1,1,1] } -> no emit
#{ DocIdPair, [1,1,1,1,1,1] } -> emit { 6, DocIdPair }
run "erl -noshell -pa ebin -s map_reduce_s -task sum_reducer -min_sum 8 -input_dir mr/09_shuffled2 -output_dir mr/10_reduced2"

# final output
`./scat mr/10_reduced2/* | perl -plne's/.*\{(.*?),(.*?)\}.*/$1 $2/;' > pairs.#{NUM}`

puts `du -sh mr`

