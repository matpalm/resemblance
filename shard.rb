require 'set'

class String

	N_GRAM_LEN = 2

	def shingles
		return @cached if @cached	
		n_grams = Set.new
		(length-N_GRAM_LEN+1).times do |i| 
			n_grams << slice(i, N_GRAM_LEN) 
		end	
    @cached = n_grams
		n_grams
	end

	def resemblance(b)
		sa = shingles
    sb = b.shingles
		numerator = (sa.intersection sb).size
		denominator = (sa.union sb).size	
		numerator.to_f / denominator  
	end

end

# read in
# two lines as file format example...

#5825 Origin Clothing, 38 Sanger St, Corowa, NSW, 2646
#5826 Taburna Pty Ltd, 38 Oxley St, Bourke, NSW, 2840

data = []
STDIN.each do |line|
	line =~ /^([0-9]*) (.*)/;
  id = $1.to_i
  name_addr = $2
	data << [id, name_addr]
end

# process
MIN_RESEMBLANCE = ARGV[0].to_f
(0...data.size).each do |i|
	((i+1)...data.size).each do |j|    
    resemblance = data[i][1].resemblance(data[j][1])
    puts [resemblance, data[i], data[j]].inspect if resemblance > MIN_RESEMBLANCE
	end
end

