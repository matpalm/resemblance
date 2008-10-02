#!/usr/bin/env ruby
# ad hoc reparsing script

require 'yaml'
results = YAML.load(File.open(ARGV[0]||'out.yaml'))

results.sort! {|a,b| b[0]<=>a[0]}

results.each do |r|
 res,a,b = r
 puts "\n#{res}\n\t#{a[1]}\n\t#{b[1]}"
end

