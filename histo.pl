#!/usr/bin/perl
use POSIX ceil;
while(<>) {
    ($r) = /^.*? .*? (.*)/;    
    $ri = ceil($r * 100);
    $buckets{$ri}++;    
}
print "$_ $buckets{$_}\n" foreach (keys %buckets);
