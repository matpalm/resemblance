#!/usr/bin/env ruby

def distance pt1, pt2
    total = 0.0
    pt1.zip(pt2).each do |pair|
        d1, d2 = pair
        diff = d1 - d2
        total += diff ** 2
    end
    Math.sqrt(total)
end

# read in all x,y from lines
pts = []
STDIN.each do |line|
    coords = line.split
    coords.delete(coords.last) # remove label
    pts << coords.map { |e| e.to_f }
end

# output pairwise distances 
(0...pts.size).each do |i|
    (i+1...pts.size).each do |j|
        a,b = pts[i], pts[j]
        puts "#{i} #{j} #{distance(a,b)}"
    end
end

