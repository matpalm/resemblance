#!/usr/bin/env ruby

raise "usage: sketch.rb HASH_SIZE SKETCH_SIZE CUTOFF_RATIO" unless ARGV.length == 3

HASH_SIZE = ARGV[0].to_i
SKETCH_SIZE = ARGV[1].to_i
CUTOFF_RATIO = ARGV[2].to_i

require 'read_data'
require 'set'
require 'sketching'
require 'shingling'

alias _puts puts
def puts msg
	_puts "#{Time.now} #{msg}"
end

documents = read_data
#puts "documents #{documents.size}"

document_ids = [] # mapping from idx in arrays to document id read from data file
document_shingles = documents.collect do |id_text|
	id,text = id_text
	document_ids << id
	text.shingles
end

#puts "made shingles"
#puts "document_ids #{document_ids.inspect}"
#document_shingles.each { |ds| puts "document_shingle #{ds.inspect}" }

sketches = Sketches.calculate_for document_shingles

#puts "made sketches"
#puts "sketches #{sketches.inspect}"

# build < xid, di > list and convert into hash xi -> ids_of_those_with_xi
x_to_i = {}
sketches.each_with_index do |s,i|
	s.sketch.each do |sk|
		x_to_i[sk] ||= Set.new
		x_to_i[sk] << i
	end
end
sketches = nil
x_to_i.reject! { |k,v| v.size==1 } # a hash that went to only one doc is useless

#puts "made x_to_i"
#puts "x_to_i.keys.size = #{x_to_i.keys.size}"
#puts "x_to_i.values.sizes = #{x_to_i.collect{|k,v| v.size}}"
#x_to_i.each {|kv| k,v=kv; puts "#{k} #{v.inspect}"}

# count the number of pairs with any xi in common
max_shingles_in_common = 0
shingles_in_common = {} # (id1,id2) #common=3 & (id1,id3) #common=4 -> { id1 => { id2 => 3, id3 => 4} }
x_to_i.keys.each do |x|
#	puts "x #{x}"
	elems_with_x = x_to_i[x].to_a
	n = elems_with_x.size
	(0...n).each do |i|
		(i+1...n).each do |j|

			pi = elems_with_x[i]
			pj = elems_with_x[j]

#			puts "i #{i} j #{j} pi #{pi.inspect} pj #{pj.inspect}"

			shingles_in_common[pi] ||= {}
			shingles_in_common_pi = shingles_in_common[pi]
			current = shingles_in_common_pi[pj]
			current ||= 0
			new_val = current + 1
			max_shingles_in_common = new_val if new_val > max_shingles_in_common
			shingles_in_common_pi[pj] = new_val

		end
	end
end
x_to_i = nil

in_common_cutoff = max_shingles_in_common / CUTOFF_RATIO

#puts "shingles in common.size #{shingles_in_common.size} max=#{max_shingles_in_common}"
#shingles_in_common.keys.each { |k| puts "#{k} #{shingles_in_common[k].inspect}" }

# convert from { id1=>{id2=>3, id3=>5}, id2=>{id3=>7}} to [ [id1,id2,3], [id1,id3,5], [id2,id3,7] ]
shingles_in_common_list = []
shingles_in_common.keys.each do |k1|
	shingles_in_common_with_k1 = shingles_in_common[k1]
	shingles_in_common_with_k1.keys.each do |k2|
		count = shingles_in_common_with_k1[k2]
		shingles_in_common_list << [k1,k2,count] if count >= in_common_cutoff
	end
end
shingles_in_common = nil

#puts "shingles_in_common_list.size #{shingles_in_common_list.size}"

# calc and display jaccard coeffs ignoring those below cutoff
shingles_in_common_list.each do |ids_count|
	a,b,count = ids_count

	id1,id2 = document_ids[a], document_ids[b]
	id1,id2 = id2,id1 if id1 > id2
	d1,d2 = documents[id1], documents[id2]
	coeff = d1.jaccard_similarity_coeff(d2)

	printf "%i %i %5.5f\n", id1,id2,coeff
#	puts "#{"-"*40}\n#{count} #{coeff} MMM\n\t#{a}\t#{d1}\n\t#{b}\t#{d2}"
end

