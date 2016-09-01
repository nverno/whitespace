require 'nokogiri'
require 'open-uri'
require 'json'

# Generate tables from tutorial website

# URL_ROOT = "http://compsoc.dur.ac.uk/whitespace/tutorial.html"

path = File.dirname(__FILE__)
file = File.open(File.join(path, "tutorial.html"))
doc = Nokogiri::HTML(file); 0
tables = doc.xpath('//table').collect { |x| x } ; 0

dat = {}
tables.each_with_index { |item, ind|
  dat[ind]={}
  dat[ind][:head] = item.css('th').collect { |x| x.content }
  dat[ind][:rows] = item.css('td').collect { |x| x.content.gsub(/[\t\n]/, ' ') }
} ; 0

dat[6][:head] = ["Example" , "Operation"]

File.open(File.join(path, "tables.json"), 'w') do |f|
  f.puts dat.to_json
end
