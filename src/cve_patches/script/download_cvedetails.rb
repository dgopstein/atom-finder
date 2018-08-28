#!/usr/bin/env ruby
require 'nokogiri'
require 'open-uri'
require 'csv'

SLEEP_DURATION = 5

def cache_and_access(url, cache_name)
  cache_filename = "cache/"+cache_name
 if File.exists?(cache_filename)
   puts "Reading from cache: #{cache_name} - #{url}"
   open(cache_filename).read
 else
   puts "Downloading to #{cache_name}: #{url}"
   result = open(url).read
   File.write(cache_filename, result)
   sleep SLEEP_DURATION
   result
 end
end

(1..2110).each do |offset|
  url = "https://www.cvedetails.com/vulnerability-list.php?vendor_id=&product_id=&version_id=&page=#{offset}&hasexp=&opdos=&opec=&opov=&opcsrf=&opgpriv=&opsqli=&opxss=&opdirt=&opmemc=&ophttprs=&opbyp=&opfileinc=&opginf=&cvssscoremin=&cvssscoremax=&year=&month=0&cweid=&order=1&trc=105464&sha=3cf9994d68386594f1283fc226cf51dad5fe72b8"

  short_name = "cve_details_%04d" % offset

  body = cache_and_access(url, short_name + ".html")

  page = Nokogiri::HTML(body)

  table = page.search('#vulnslisttable')

  rows = []
  descriptions = []

  table.search('tr').map do |tr|
    cells = tr.search('th, td')
    if cells.length > 1
      rows << cells.map(&:text).map(&:strip).to_a
    else
      descriptions << cells.first.text.strip
    end
  end

  rows_desc = rows.zip(["Description"] + descriptions).map{|(row, desc)| row + [desc]}

  CSV.open("data/"+short_name+".csv", 'w') do |csv|
    rows_desc.each { |row| csv << row }
  end
end
