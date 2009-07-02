#!/usr/bin/env ruby

require 'fileutils'

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

def extract_exact_dups type, prep_type, num_entries, num_files
	# collate into exact dups
	run "head -n #{num_entries} ../#{type} | perl -plne'tr/A-Z/a-z/' | erl -noshell -pa ebin -s prepare -parser #{prep_type} -num_files #{num_files} -output_dir mr/#{type}"
	# { 1024, 'bobs cafe' }
	run "erl -noshell -pa ebin -s map_reduce_s -task swap_key_and_value -input_dir mr/#{type} -output_dir mr/#{type}.swap"
	# { 'bobs cafe', 1024 }
	run "erl -noshell -pa ebin -s shuffle -input_dir mr/#{type}.swap -output_dir mr/#{type}.swap.reduce"
	# { 'bobs cafe', [1024,1025] }

	# extract combos
	run "erl -noshell -pa ebin -s map_reduce_s -task combos -input_dir mr/#{type}.swap.reduce -output_dir mr/#{type}.swap.combos"
	# { {1024,1025}, 1 }
	run "erl -noshell -pa ebin -s map_reduce_s -task add_type_to_value -type #{type} -input_dir mr/#{type}.swap.combos -output_dir mr/#{type}.combos"
	# { {1024,1025}, {name,1} }

	return if type=='phones'

	# filter single values
	# { 'bobs cafe', [1024,1025,1045] }
	run "erl -noshell -pa ebin -s map_reduce_s -task filter_single_value -input_dir mr/#{type}.swap.reduce -output_dir mr/#{type}.unique.vk"
	# { 'bobs cafe', 1024 }
	run "erl -noshell -pa ebin -s map_reduce_s -task swap_key_and_value -input_dir mr/#{type}.unique.vk -output_dir mr/#{type}.unique"
	# { 1025, 'bobs cafe' }

	# store master -> slave ids
	# { 'bobs cafe', [1024,1025,1045] }
	run "erl -noshell -pa ebin -s map_reduce_s -task use_first_value_as_key -input_dir mr/#{type}.swap.reduce -output_dir mr/#{type}.master_slaves"
	# { 1024, [1025,1045] }

  # want to keep; combos - for exact dups
	#               master_slaves, unique - for sketch deduping
end

def sketch_dedup type, num_files
	run "erl -noshell -pa ebin -s map_reduce_s -task shingler -shingle_size 3 -input_dir mr/#{type}.unique -output_dir mr/#{type}.shingles"
end

def extract_exact_duplicates
	extract_exact_dups 'phones',    'prepare_id_num',  NUM_ENTRIES, NUM_FILES
	extract_exact_dups 'names',     'prepare_id_text', NUM_ENTRIES, NUM_FILES
	extract_exact_dups 'addresses', 'prepare_id_text', NUM_ENTRIES, NUM_FILES
	# collect resemblances into a single dir
	FileUtils.mkdir_p 'mr/resems.tmp'
	['phones','names','addresses'].each do |type|
		dir = "mr/#{type}.combos"
		Dir.new(dir).entries.each do |file|
			next if file == '.' || file == '..'
			FileUtils.mv "#{dir}/#{file}", "mr/resems.tmp/#{type}#{file}"
		end
	end
	run "erl -noshell -pa ebin -s shuffle -input_dir mr/resems.tmp -output_dir mr/resems"
end

def calculate_sketch_near_duplicates
	sketch_dedup 'names', NUM_FILES
end

NUM_ENTRIES = ARGV.shift || "10"
#SRC_FILE = ARGV.shift || "../name_addr"
NUM_FILES = ARGV.shift || "10"

msg = "NUM_ENTRIES=#{NUM_ENTRIES} NUM_FILES=#{NUM_FILES}"
log msg

#run "rm -rf mr/*"

extract_exact_duplicates
calculate_sketch_near_duplicates

# for very end, 
#run "erl -noshell -pa ebin -s reducer -task combine_nap -input_dir mr/resems -output_file devnull > ids_name_addr_phone"

exit 0

=begin

	# prepare data
	# { doc_id, "text of document here" }
	run "head -n #{NUM} #{SRC_FILE} | perl -plne'tr/A-Z/a-z/' | erl -noshell -pa ebin -s prepare -parser prepare_id_text -num_files #{FILES} -output_dir mr/01_prepared"

	# map to { doc_id, [shingles] } 
	run "erl -noshell -pa ebin -s map_reduce_s -task shingler -shingle_size 3 -input_dir mr/01_prepared -output_dir mr/02_id_to_shingles"

# determine most frequent shingles
# output { shingle, freq } 
=begin
run "erl -noshell -pa ebin -s map_reduce_s -task emit_values -input_dir mr/02_id_to_shingles -output_dir mr/02_2_shingle_to_1"
run "erl -noshell -pa ebin -s shuffle -input_dir mr/02_2_shingle_to_1 -output_dir mr/02_3_shingle_to_1_shuffled"
run "erl -noshell -pa ebin -s map_reduce_s -task sum -input_dir mr/02_3_shingle_to_1_shuffled -output_dir mr/02_4_shingle_freq"
run "erl -noshell -pa ebin -s map_reduce_s -task top_N -num_to_keep 10 -input_dir mr/02_4_shingle_freq -output_dir mr/02_5_top_shingle_freq"
run "erl -noshell -pa ebin -s reducer -task top_N -num_to_keep 10 -input_dir mr/02_5_top_shingle_freq -output_file mr/02_6_most_freq"
# remove shingles that are in the most common set
# input_files { doc_id, [shingles] } output_files { doc_id, [shingles] }
run "erl -noshell -pa ebin -s map_reduce_s -task remove_common_shingles -common_file mr/02_6_most_freq -input_dir mr/02_id_to_shingles -output_dir mr/03_id_to_uncommon_shingles"


# identity step when not removing freq shingles
run "cp -r mr/02_id_to_shingles mr/03_id_to_uncommon_shingles"

# map to { sketch_value, doc_id } 
run "erl -noshell -pa ebin -s map_reduce_s -task sketcher -input_dir mr/03_id_to_uncommon_shingles -output_dir mr/04_sketches"

# shuffle on sketch value -> { SketchValue, [DocId, DocId, ... ] }
run "erl -noshell -pa ebin -s shuffle -input_dir mr/04_sketches -output_dir mr/05_shuffled"

# emit all combos; 
#  { 123, [1,2,3] } emits {[1,2],1} {[1,3],1} {[2,3],1} 
#  { 123, [1] } emits nothing
run "erl -noshell -pa ebin -s map_reduce_s -task combos -input_dir mr/05_shuffled -output_dir mr/06_reduced"

# shufle on doc id pairs { DocIdPair, [1,1,1, ...  ] }
run "erl -noshell -pa ebin -s shuffle -input_dir mr/06_reduced -output_dir mr/07_shuffled"

# sum docid pair freqs, 
# emits freq first, not doc id pair 
# only emits if freq > 8
#{ DocIdPair, [1,1,1] } -> no emit
#{ DocIdPair, [1,1,1,1,1,1] } -> emit { 6, DocIdPair }
run "erl -noshell -pa ebin -s map_reduce_s -task sum -min_sum 8 -input_dir mr/07_shuffled -output_dir mr/08_reduced"

# final output
`./scat mr/08_reduced/* | perl -plne's/.*\{(.*?),(.*?)\}.*/$1 $2/;' > pairs.#{NUM}`

puts `du -sh mr`
=end
