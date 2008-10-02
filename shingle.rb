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

  def invalidate_cache
		@cached = nil
  end

end

MIN_RESEMBLANCE = ARGV[0].to_f
WINDOW_SIZE = 100 # following records

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
require 'yaml'
results =  []
comparisons = 0
(0...data.size).each do |i|
  j_to = i + WINDOW_SIZE
  j_to = data.size if j_to > data.size
	((i+1)...j_to).each do |j| 
		comparisons += 1
    resemblance = data[i][1].resemblance(data[j][1])
    results << [resemblance, data[i], data[j]] if resemblance > MIN_RESEMBLANCE
	end  
  data[i][1].invalidate_cache # need to free some memory!!
end
puts "#comparisons #{comparisons}"

YAML.dump(results,File.open(ARGV[1]||'out.yaml',"w")) 