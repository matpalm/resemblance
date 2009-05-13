# read in
# two lines as file format example...

#5825 Origin Clothing, 38 Sanger St, Corowa, NSW, 2646
#5826 Taburna Pty Ltd, 38 Oxley St, Bourke, NSW, 2840

def read_data stream=STDIN
	data = {}
	stream.each do |line|
		line =~ /^([0-9]*) (.*)/;
		id = $1.to_i
		name_addr = $2
		raise "duplicate id #{id}?" if data[id]
		data[id] = name_addr
	end
	data
end
