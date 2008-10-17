#!/usr/bin/env ruby

def read_from file
    dists = []
    IO.read(file).each do |line|
        i,j,dist = line.split
        dists << dist.to_f
    end
    dists
end

dists1 = read_from ARGV[0]
dists2 = read_from ARGV[1]

total = 0
dists1.zip(dists2).each do |dist_pair|
    diff = dist_pair[0] - dist_pair[1]
    total += diff ** 2
end
puts Math.sqrt(total)
