#!/usr/bin/env ruby

require 'fileutils'

def log msg
	`echo #{msg} >> stats.out`
end

def run(command)
	@cmd += 1
	now = Time.now
	puts "#{now} (#{now-@last}sec) #{@cmd} #{command}"
  log "S #{@cmd}"
	`#{command}`
	log "E #{@cmd} DU #{`du -sh mr`.chomp}"
	@last = now
end

@cmd = 0
@last = Time.now

NUM_ENTRIES = ARGV.shift || "10"
SRC_FILE = ARGV.shift || "../name_addr"
NUM_FILES = ARGV.shift || "10"

NAME_WEIGHT = 4
ADDR_WEIGHT = 5
PHONE_WEIGHT = 1

def sketch_dedup type
	run "cat #{type}.unique | erl -noshell -pa ebin -s prepare -parser prepare_id_text -num_files #{NUM_FILES} -output_dir mr/#{type}.unique"
	run "erl -noshell -pa ebin -s map_reduce_s -tasks shingler sketcher -shingle_size 3 -input_dirs mr/#{type}.unique -output_dir mr/#{type}.sketches"
	run "erl -noshell -pa ebin -s shuffle -input_dirs mr/#{type}.sketches -output_dir mr/#{type}.shuffled"
	run "erl -noshell -pa ebin -s map_reduce_s -tasks combos -input_dirs mr/#{type}.shuffled -output_dir mr/#{type}.all_combos"
	run "erl -noshell -pa ebin -s shuffle -input_dirs mr/#{type}.all_combos -output_dir mr/#{type}.all_combos_shuffled"
	# { {123,234}, [1,1,1,1,1] }
	run "erl -noshell -pa ebin -s map_reduce_s -tasks sum emit_key_as_pair -min_sum 8 -input_dirs mr/#{type}.all_combos_shuffled -output_dir mr/#{type}.combos_pairs"
	# { 123, 234 }

	# calculate jaccard similiarity for the sketch resulting combo pairs
  # bit hacky, wouldn't be surprised if this blows memory
	# might have to load unique.all or combo_pairs into dets 
  # or do a map reduce join
  # 
	run "cat mr/#{type}.unique/* > mr/#{type}.unique.all" # hack!
	run "cat mr/#{type}.combos_pairs/* > mr/#{type}.combos_pairs.all" # hack2!
	run "erl -noshell -pa ebin -s pair_to_jaccard -shingle_size 3 -type #{type} -id_name mr/#{type}.unique.all -id_pairs mr/#{type}.combos_pairs.all -output_file mr/#{type}.sketch.unexploded.result "
end

####

# makes [nap,names,addresses,phones].combo.ids 
def extract_exact_duplicates
	run "head -n #{NUM_ENTRIES} ../name_addr | ../split.rb single_export"
	run "sort -k2 -t\\| nap | ../find_dups.rb nap" # nap.unique, nap.combo.ids & nap.dup.ids

	run "cat nap.unique | ../split.rb" # names, addresses, phones
	['names', 'addresses', 'phones'].each do |type|
		run "sort -k2 -t\\| #{type} | ../find_dups.rb #{type}" # type.unique, type.combo.ids & type.dup.ids
		run "cat #{type}.dup.ids | erl -noshell -pa ebin -s prepare -parser prepare_id_list_type -type #{type} -num_files 1 -output_file mr/result/#{type}.exact.result"
	end

end

def calculate_sketch_near_duplicates 
	['names','addresses'].each do |type|
		sketch_dedup type # makes type.sketch.result
	end
end

def explode_sketch_results
	# explode combos from mr/type.sketch.unexploded.result 
  # using type.dup.ids
  # to make mr/type.sketch.exploded.result
	# like end of sketch_dedup this is also a bit hacky, again might be better as dets load or m/r join
	['names','addresses'].each do |type|
		run "cat #{type}.dup.ids | erl -noshell -pa ebin -s prepare -parser prepare_id_nums -num_files 1 -output_file mr/#{type}.dup.ids.all"
		run "erl -noshell -pa ebin -s explode_combos -dup_ids mr/#{type}.dup.ids.all -input_file mr/#{type}.sketch.unexploded.result -output_file mr/result/#{type}.sketch.result"
	end
end

def combine_results
	# merge..
	#  mr/result/<type>.exact.result ; name/address/phone
	#  mr/result/<type>.sketch.result ; name/address
	run "erl -noshell -pa ebin -s shuffle -input_dirs mr/result -output_dir mr/final_result"
	run "cat mr/final_result/* > mr/final_result.all"
	run "erl -noshell -pa ebin -s calculate_nap -file mr/final_result.all -n #{NAME_WEIGHT} -a #{ADDR_WEIGHT} -p #{PHONE_WEIGHT} | sort -nrk3 > final_res"
end

####

msg = "NUM_ENTRIES=#{NUM_ENTRIES} NUM_FILES=#{NUM_FILES} NAME_WEIGHT=#{NAME_WEIGHT} ADDR_WEIGHT=#{ADDR_WEIGHT} PHONE_WEIGHT=#{PHONE_WEIGHT}"
log msg

# TODO: where are combo.ids used? can we use dup.ids there instead??
run "rm -rf  names* addresses* phones* nap* mr/*"
run "mkdir mr/result"
extract_exact_duplicates
calculate_sketch_near_duplicates
explode_sketch_results
combine_results

#TODO: wc version of scat, look for header and then skip that many bytes

