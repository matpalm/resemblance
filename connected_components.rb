#!/usr/bin/env ruby

require 'rubygems'
require "rgl/adjacency"
require "rgl/traversal"
require "rgl/connected_components"

incident_edge_sum = {}
incident_edge_sum.default = 0

g = RGL::AdjacencyGraph.new

STDIN.each do |line|
	id1,id2,res = line.split
	id1,id2,res = id1.to_i, id2.to_i, res.to_f
	incident_edge_sum[id1] += res
	incident_edge_sum[id2] += res
	g.add_vertex id1
	g.add_vertex id2
	g.add_edge id1, id2
end

g.each_connected_component do |vertexs| 
	max_sum = 0
	max_vert = nil
	vertexs.each do |vertex|
		if incident_edge_sum[vertex] > max_sum
			max_sum = incident_edge_sum[vertex]
			max_vert = vertex
		end
	end
	vertexs.delete max_vert
	puts "#{max_vert} #{vertexs.join(' ')}"
end
