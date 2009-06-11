#!/usr/bin/env ruby
def convert str
	str.gsub 's',''
	h,m,s = 0
	s = str.to_i
	if s > 60
		m = s / 60
		s -= m * 60
	end
	if m > 60
		h = m / 60
		m -= h * 60
	end
	printf("%s = ",str)
	printf("%ih ",h) if h>0
	printf("%im ",m) if m>0
	printf("%is ",s) if s>0
	puts
end

ARGV.each { |s| convert s }

