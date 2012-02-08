#!/usr/bin/env ruby
require 'set'
require File.dirname(__FILE__)+'/universal_hash'

N_GRAM_LEN = 3
SKETCH_SIZE = 10

class String

  def shingles
    n_grams = Set.new
    (length-N_GRAM_LEN+1).times do |i|
      n_grams << slice(i, N_GRAM_LEN)
    end
    n_grams
  end

  SKETCH_MAX = (2**61)-1
  SKETCH_SALT = 29764293764

  def sketch
    sketch_using_universal_hash
  end

  def sketch_using_salted_hash
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
  
  def sketch_using_universal_hash
    @@hashes ||= SKETCH_SIZE.times.collect { UniversalHash.build }
    @@hashes.collect { |h| h.hash(self) }
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
#  warn "line"
  line =~ /^(\d*)\t(.*)/
  id, text = $1, $2
#  warn "id=[#{id}] text=[#{text}]"
  shingles = text.shingles
#  warn "shingle #{shingles.inspect}"
  mins = nil
  text.shingles.each do |shingle|	
    sketch_values = shingle.sketch
#    warn "for shingle [#{shingle}] sketch_values #{sketch_values.inspect}"
    if mins.nil?
      mins = sketch_values
    else
      mins = mins.pairwise_min sketch_values
    end
#    warn "new mins #{mins.inspect}"
  end
  mins.each { |min| puts "#{min}\t#{id}" }
end
	
