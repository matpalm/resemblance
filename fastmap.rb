#!/usr/bin/env ruby

class Distances

    def initialize
        @data = []
		@size = 1
    end

    def set_distance i,j,value
	    @size += 1 if i==0
        @data[i] ||= []
        @data[i][j] = value
    end

    def distance i,j
        return 0 if i==j
#        puts "distance #{i} #{j}"
        i,j = j,i if i>j
        @data[i][j]
    end

    def furthest_from x
        furthest_pt = nil
        furthest_dist = -1
        @size.times do |y|
            dist = distance x,y
            if dist > furthest_dist
                furthest_pt, furthest_dist = y, dist
            end
        end
        furthest_pt
    end

=begin
    def pick_end_points
        a = furthest_from 0 # pick furthest from artibtrary point
        b = furthest_from a # pick furthest from that point        
        [a, b] # done
    end
=end
    
    def projection_distances a, b
        result = []

        # cache dab stuff, used a number of times
        dab = distance a,b        
        dab_sqrd = dab ** 2
        dab2 = dab * 2

        @size.times do |i|
            if i==a
                # boundary case; projecting start pt onto itself, so distance to start is zero
                result << 0.0
            elsif i==b
                # boundary case; projecting end pt onto itself, so distance to start if full length
                result << dab
            else
                # project other point onto line between a and b
                dai = distance a,i
                dbi = distance b,i
                dist = ( dai**2 + dab_sqrd - dbi**2 ) / dab2
#                puts "a#{a} b#{b} i#{i} dab=#{dab} dai=#{dai} dbi=#{dbi} => dist=#{dist}"
                result << dist 
            end
        end

        result
    end

    def distances_orthogonal_to x_projection
        data_ = Distances.new
        (0...@size).each do |i|
            xi = x_projection[i]
            (i+1...@size).each do |j|
                dist = distance i,j
                raise "i=#{i} j=#{j}" if dist==nil
                xj = x_projection[j]
                xixj = xi - xj
                diff = dist**2 - xixj**2
                diff = 0 if diff<0 # HUGE hack to avoid rounding errors for xixj to -1e-100
#                puts "dist=#{dist} xixj=#{xixj} diff=#{diff}"
                d_ = Math.sqrt( diff )
                data_.set_distance i,j,d_
#                printf("%d %d dist=%3.3f xi=%3.3f xj=%3.3f  d_=%3.3f\n", i,j,dist,xi,xj,d_)
            end
        end
        data_
    end

    def ppp
        require 'pp'
        pp @data
    end
    
end

raise "fastmap.rb <DIMENSION>" unless ARGV.size==1

NUM_DIMENSIONS = ARGV[0].to_i

projections = []

# parse initial distances from stdin
# additional scan for biggest distance
data = Distances.new
largest_dist = -1
largest_dist_pts = nil
STDIN.each do |line|
    i,j,distance = line.split    
    i,j,distance = i.to_i, j.to_i, distance.to_f
    if distance > largest_dist
        largest_dist = distance
        largest_dist_pts = [i,j]
    end
    data.set_distance i.to_i, j.to_i, distance
end

(NUM_DIMENSIONS).times do |dimension|
    # pick a line from two points that are far apart from each other
    a,b = largest_dist_pts

    # project all points onto this line 
    x_projection_distances = data.projection_distances(a,b)
    projections << x_projection_distances

    # recalc distances orthogonal to ab
    data = data.distances_orthogonal_to(x_projection_distances) unless dimension == NUM_DIMENSIONS-1
end

# "rotate" collected projection for the purpose of plotting
# ie one line represents a point instead of one line representing a dimension
dim = 0
while(!projections.first.empty?) do
    projections.each { |projection| printf("%20.20f ",projection.shift) }
    puts "#{dim}"
    dim += 1
end
