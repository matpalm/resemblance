#!/usr/bin/env ruby
raise "usage: cat data | ./find_dups.rb <prefix>" unless ARGV.length==1

@last_data = nil
@dup_ids = []
PREFIX = ARGV.first

@unique_ids_file = File.open("#{PREFIX}.unique",'w')
@dup_ids_file = File.open("#{PREFIX}.dup.ids","w")
@id_combos_file = File.open("#{PREFIX}.combos.ids","w")

def dump
		ids = @dup_ids

		master_id = ids.shift
		@unique_ids_file.puts "#{master_id}|#{@last_data}"

		if not ids.empty?
			output_ids = [master_id] + ids
			@dup_ids_file.puts output_ids.join(' ')
		end

		ids.unshift master_id
		while !ids.empty?
			id1 = ids.shift
			ids.each do |id2| 
				id1,id2 = id2,id1 if id1 > id2
				@id_combos_file.puts "#{id1} #{id2}" 
			end
		end

		@dup_ids = []
end

STDIN.each do |line|
	line.chomp!
	line =~ /(.*?)\|(.*)/
	id,data = $1,$2
	dump if @last_data != data and @last_data != nil
	@last_data = data
	@dup_ids << id
end

dump
@dup_ids_file.close
@id_combos_file.close
