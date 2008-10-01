#!/usr/bin/env ruby
# ad hoc reparsing script

require 'yaml'
results = YAML.load(File.open(ARGV[0]||'out.yaml'))

diffs = []
results.each do |r|
 res,a,b = r
 idx_diff = (a[0] - b[0]).abs
 diffs << idx_diff
 puts "#{res} #{idx_diff}"
end

puts "mean #{diffs.sort[diffs.size/2]}"
