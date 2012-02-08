cat test.data | ./sketch.rb | sort -n | ./exploded_combos.rb | sort | uniq -c
