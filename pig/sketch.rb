#!/usr/bin/env ruby
require 'set'

N_GRAM_LEN = 5
SKETCH_SIZE = 10
SKETCH_MAX = (2**61)-1
SKETCH_SALT = 29764293764

class String
  
  def shingles
    n_grams = Set.new
    (length-N_GRAM_LEN+1).times do |i|
      n_grams << slice(i, N_GRAM_LEN)
    end
    n_grams
  end
  
  def sketch
    x = self.hash % SKETCH_MAX
    y = (x * SKETCH_SALT) % SKETCH_MAX
    idxs = [x]
    (SKETCH_SIZE-1).times do |i|
      x = (x+y) % SKETCH_MAX 
      y = (y+1) % SKETCH_MAX
      idxs << x
    end
    idxs
  end
  
end

class Array
  def pairwise_min other
    raise 'fail' unless self.length == other.length
    (0...length).collect do |idx|
      self[idx] < other[idx] ? self[idx] : other[idx]
    end
  end
end

STDIN.each do |line|
  line =~ /^(\d+)\|(.*)/
  id, text = $1, $2
  shingles = text.shingles
  mins = nil
  text.shingles.each do |shingle|	
    sketch_values = shingle.sketch
    if mins.nil?
      mins = sketch_values
    else
      mins = mins.pairwise_min sketch_values
    end
  end
  mins.each { |min| puts "#{min}\t#{id}" }
end
	
