#!/usr/bin/ruby

# Transform the human-formatted random-examples output txt files and turn them
# into a csv. In general this shouldn't be necessary, since random examples can
# be written direcly to a csv, but I wanted to record the ones we used
# specifically for validation in the paper, and the only record we had was in
# the human-readible csvs.

require 'csv'

def new_file
  $type=nil

  $url = nil
  $code = ''
  $i = 0

  $example_num = 0
end

def parse_line
  # Classify each line
  if $line =~ /^https.*/
    $type = :url
  elsif $line =~ /^-+$/
    $type = :delim
  else
    if $type != :code && $line.empty?
      $type = :blank
    else
      $type = :code
    end
  end
end

def record_line
  case $type
  when :url
    $url = $line.strip
  when :code
    $code += $line
  when :delim
    $examples << [$atom_type, $i, $url, $url.split(/\//).last, $code.strip]
    $i += 1

    $code = ''
    $example_num += 1
  end
end

#puts "#{type} | #{$line}"

def write_csv(filename)
  CSV.open(filename, 'w') do |csv|
    csv << ['atom', 'id', 'url', 'file_line', 'code']
    $examples.each do |ex|
      csv << ex
    end
  end
end

def main
  $examples = []

  Dir['*.txt'].map do |filename|
    filename_no_ext = filename.split('.')[0]
    $atom_type = filename_no_ext.split('-').map(&:capitalize).join(' ')

    file = File.open(filename).read
    new_file

    file.lines.map do |l|
        $line = l
        parse_line
        record_line
    end

  end

  write_csv('csvs/classifier_examples.csv')
end

main
