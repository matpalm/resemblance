# output resemblance and average distance between idxes with that resemblance

totals = {}
totals.default = 0

counts = {}
counts.default = 0

STDIN.each do |line|
    a,b,resem = line.split ' '
    resem=resem.to_f; a=a.to_i; b=b.to_i;

    bucket = (resem * 100).to_i
    diff = b - a
    raise "wtf #{line}" if diff<0       

    totals[bucket] += diff
    counts[bucket] += 1
    #puts "#{resem} #{a} #{b} #{bucket} #{diff} " 
end

averages = {}
totals.keys.each do |k|
    averages[k] = totals[k].to_f / counts[k]
end

def to_s_sorted hash
    out = []
    hash.keys.sort.each { |k| out << "#{k}=>#{hash[k].inspect}" }
    puts out.join(', ')
end

def gnuplot_out hash, counts
    hash.keys.sort.each { |k| puts "#{k} #{hash[k]} #{counts[k]}" }    
end

#gnuplot_out totals, counts
gnuplot_out averages, counts

