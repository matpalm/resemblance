p#!/usr/bin/env ruby

raise "usage: sketch.rb HASH_SIZE SKETCH_SIZE CUTOFF" unless ARGV.length == 3

HASH_SIZE = ARGV[0].to_i
SKETCH_SIZE ||= ARGV[1].to_i
CUTOFF ||= ARGV[2].to_i

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

#puts "x_to_i.keys.size = #{x_to_i.keys.size}"
#puts "x_to_i.values.sizes = #{x_to_i.collect{|k,v| v.size}}"
#x_to_i.each {|kv| k,v=kv; puts "#{k} #{v.inspect}"}

# count the number of pairs with any xi in common
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
			shingles_in_common[pi][pj] ||= 0 # move to default?
			shingles_in_common[pi][pj] += 1

#			shingles_in_common[pj] ||= {}
#			shingles_in_common[pj][pi] ||= 0 # move to default?
#			shingles_in_common[pj][pi] += 1
		end
	end
end
x_to_i = nil

#puts "shingles in common.size #{shingles_in_common.size}"
#shingles_in_common.keys.each { |k| puts "#{k} #{shingles_in_common[k].inspect}" }

# convert from { id1=>{id2=>3, id3=>5}, id2=>{id3=>7}} to [ [id1,id2,3], [id1,id3,5], [id2,id3,7] ]
MIN_COUNT = 1
shingles_in_common_list = []
shingles_in_common.keys.each do |k1|
	shingles_in_common_with_k1 = shingles_in_common[k1]
	shingles_in_common_with_k1.keys.each do |k2|
		count = shingles_in_common_with_k1[k2]
		shingles_in_common_list << [k1,k2,count] if count >= MIN_COUNT
	end
end
shingles_in_common = nil

#puts "shingles_in_common_list.size #{shingles_in_common_list.size}"

# find max in common
max = -1
shingles_in_common_list.each do |ids_count|
	count = ids_count[2]
	max = count if count > max
end

#puts "max in common = #{max}"

# calc and display jaccard coeffs ignoring those below cutoff
#num_checked = 0
count_cutoff = max / CUTOFF 
shingles_in_common_list.each do |ids_count|
	a,b,count = ids_count
	next if count < count_cutoff
	#num_checked += 1

	id1,id2 = document_ids[a], document_ids[b]
	d1,d2 = documents[id1], documents[id2]
	coeff = d1.jaccard_similarity_coeff(d2)

	printf "%i %i %5.5f\n", id1,id2,coeff
#	puts "#{"-"*40}\n#{count} #{coeff} MMM\n\t#{a}\t#{d1}\n\t#{b}\t#{d2}"
end

#puts "num_checked #{num_checked} / total #{shingles_in_common_list.size}"
