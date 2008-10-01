#!/usr/bin/env ruby
# diy cat of a previous runs yaml output
require 'yaml'
YAML.load(File.open(ARGV[0]||'out.yaml')).each { |r| puts r.inspect }
