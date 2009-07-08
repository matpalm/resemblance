#!/usr/bin/env ruby
# determines the node in a complete graph that has the highest sum of incident edge weights
# note: doesn't assert that graph is complete!!
# this node, in deuduping, can be treated as the canonical representation for the clique

incident_edges_sum = {}
incident_edges_sum.default = 0

STDIN.each do |line|
	id1,id2,res = line.chomp.split
	id1,id2,res = id1.to_i, id2.to_i, res.to_f
	incident_edges_sum[id1] += res
	incident_edges_sum[id2] += res
end

max_sum = 0
max_node = nil
incident_edges_sum.each do |node, sum|
	if sum > max_sum
		max_sum = sum
		max_node = node
	end
end

puts "#{max_node} #{max_sum}"
