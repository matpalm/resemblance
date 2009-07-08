#!/usr/bin/env ruby

tmpfile = "/dev/shm/#{$$}.tmp"
tmp = File.new(tmpfile, "w")
tmp.puts "graph {"
STDIN.each do |line|
	id1,id2,res = line.chomp.split
	res = res.to_f
	tmp.puts "#{id1} -- #{id2} [ label = \"#{sprintf("%1.2f",res)}\" ];" if res > 0.6
end
tmp.puts "}"
tmp.close
`dot -Tpng #{tmpfile} > final_res.png`
