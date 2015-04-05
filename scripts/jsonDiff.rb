#!/usr/bin/env ruby

require 'json'
require 'hashdiff'

# Make sure we are running in correct directory
unless $0.start_with? "scripts/"
    abort "Please run as scripts/#{$0.split("/")[-1]}"
end

file = ARGV[0]
if file.nil?
    abort "Usage: #{__FILE__} <path-to-json-file>"
end

# execute following command in subshell, and save stdout to variable
json = `cabal configure --verbose=0 && cabal run --verbose=0 #{file} testParse`
if json.nil?
    abort "Bad json file"
end

orig = JSON.parse(File.open(file).read)
new = JSON.parse(json)

diff = HashDiff.diff(orig, new)
puts diff
