#!/usr/bin/env bash
set -x
rm -rf mr_*
time head -n $1 ../name_addr | erl -noshell -pa ebin -s prepare main -output_dir mr_prepared
time erl -noshell -pa ebin -s map_reduce main sketch_mapper -input_files mr_prepared/* -output_dir mr_mapped
time erl -noshell -pa ebin -s shuffle main -input_files mr_mapped/* -output_dir mr_shuffled
time erl -noshell -pa ebin -s map_reduce main reducer -input_files mr_shuffled/* -output_dir mr_reduced
#erl -noshell -pa ebin -s map_reduce main value_one_mapper -input_files mr_reduced/* -output_dir mr_mapped2 now done by reducer
time erl -noshell -pa ebin -s shuffle main -input_files mr_reduced/* -output_dir mr_shuffled2
time erl -noshell -pa ebin -s map_reduce main sum_reducer -input_files mr_shuffled2/* -output_dir mr_reduced2
time erl -noshell -pa ebin -s shuffle main -input_files mr_reduced2/* -output_dir mr_shuffled3

