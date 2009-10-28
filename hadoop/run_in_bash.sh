cat test.data.orig | ./sketch.rb | sort -n | ./exploded_combos.rb | sort | uniq -c
