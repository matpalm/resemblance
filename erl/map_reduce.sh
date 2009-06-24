#!/usr/bin/env bash
set -x
rm -rf mr_*

# prepare data by spreading if over N files
time head -n $1 ../name_addr | erl -noshell -pa ebin -s prepare main -output_dir mr_prepared

# for each line of input emit sketch values for each shingle { SketchValue, DocId }
time erl -noshell -pa ebin -s map_reduce -task sketch_mapper -input_dir mr_prepared -output_dir mr_mapped

# shuffle on sketch value -> { SketchValue, [DocId, DocId, ... ] }
time erl -noshell -pa ebin -s partition -input_dir mr_mapped -output_dir mr_partitioned
time erl -noshell -pa ebin -s map_reduce -task sort_collate -input_dir mr_partitioned -output_dir mr_shuffled_seperate
time erl -noshell -pa ebin -s merge -input_dir mr_shuffled_seperate -output_dir mr_shuffled

# emit all combos; { 123, [1,2,3] } emits {[1,2],1} {[1,3],1} {[2,3],1} 
time erl -noshell -pa ebin -s map_reduce -task reducer -input_dir mr_shuffled -output_dir mr_reduced

# shufle on doc id pairs { DocIdPair, [1,1,1, ...  ] }
time erl -noshell -pa ebin -s partition -input_dir mr_reduced -output_dir mr_partitioned2
time erl -noshell -pa ebin -s map_reduce -task sort_collate -input_dir mr_partitioned2 -output_dir mr_shuffled_seperate2
time erl -noshell -pa ebin -s merge -input_dir mr_shuffled_seperate2 -output_dir mr_shuffled2

# sum docid pair freqs, 
# emits freq first, not doc id pair 
# only emits if freq > 5
#{ DocIdPair, [1,1,1] } -> no emit
#{ DocIdPair, [1,1,1,1,1,1] } -> emit { 6, DocIdPair }
time erl -noshell -pa ebin -s map_reduce -task sum_reducer -min_sum 10 -input_dir mr_shuffled2 -output_dir mr_reduced2

# collect doc id pairs with same frequency, needs a filter as well?
#time erl -noshell -pa ebin -s partition -input_files mr_reduced2/* -output_dir mr_partitioned3
#time erl -noshell -pa ebin -s map_reduce main shuffle -input_files mr_partitioned3/* -output_dir mr_shuffled3

