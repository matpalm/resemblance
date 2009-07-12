#!/usr/bin/env ruby

OUTFILE = ARGV[0] || 'final_res.png'
tmpfile = "/dev/shm/#{$$}.tmp"
tmp = File.new(tmpfile, "w")
tmp.puts "graph {"
STDIN.each do |line|
	id1,id2,res = line.chomp.split
	res = res.to_f
	tmp.puts "#{id1} -- #{id2} [ label = \"#{sprintf("%1.2f",res)}\" ];"
end
tmp.puts "}"
tmp.close
`dot -Tpng #{tmpfile} > #{OUTFILE}`
`rm #{tmpfile}`
