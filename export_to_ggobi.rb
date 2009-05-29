#!/usr/bin/env ruby

require 'cgi'

records = []
STDIN.each do |line|
    line =~ /(.*)"(.*)"/
    records << { :points => $1.split(' '), :label => $2.strip }
end

puts <<EOF
<?xml version="1.0"?>
<!DOCTYPE ggobidata SYSTEM "ggobi.dtd">
<ggobidata count="1">
<data>
EOF

number_dimensions = records.first[:points].size

puts "<variables count=\"#{number_dimensions}\">"
number_dimensions.times do |d|
    puts " <realvariable name=\"dim#{d}\"/>"
end
puts "</variables>"

puts "<records count=\"#{records.size}\" glyph=\"fc 1\" color=\"1\">"

records.each do |record|
    puts "<record label=\"#{CGI.escapeHTML record[:label]}\">"
    record[:points].each { |pt| puts "<real>#{pt}</real>" }
    puts "</record>"
end

puts <<EOF
</records>
</data>
</ggobidata>
EOF

