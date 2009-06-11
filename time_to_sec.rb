#!/usr/bin/env ruby
def convert_to_sec p
	time = p[0,p.length-1].to_i
  unit = p.chars.to_a.last.downcase
	seconds_in = {'h' => 60*60, 'm' => 60, 's' => 1 }
	raise "unknown unit #{unit}" unless seconds_in.include? unit
	time * seconds_in[unit]
end

puts ARGV.collect{ |arg| convert_to_sec(arg) }.inject{|a,v| v+a}
