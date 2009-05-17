class Array
	def chunks chunk_size
		chunks = (1..chunk_size).collect { [] }
		idx = 0
		while any?
			chunks[idx % chunk_size] << shift
			idx += 1
		end
		chunks
	end
end
