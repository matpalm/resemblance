#!/usr/bin/env ruby

def emit_combos list
  while not list.empty?
    first = list.shift
    list.each { |other| puts "#{first} #{other}" }
  end
end

last_sketch_value = nil
sketchs_in_common = []
STDIN.each do |line|
  sketch_value, doc_id = line.chomp.split "\t"
  if sketch_value != last_sketch_value and not last_sketch_value.nil?
    emit_combos sketchs_in_common.sort if sketchs_in_common.size > 1
    sketchs_in_common.clear
  end
  sketchs_in_common << doc_id 
  last_sketch_value = sketch_value 
end
