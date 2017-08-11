#!/usr/bin/env ruby
# Base-36 timestamp words usable in the next year or so.

ts = {}
open('/usr/share/dict/words') do |f|
  f.each_line do |l|
    t = Time.at(l.downcase.tr('^a-z','').ljust(6,'0').to_i(36))
    ts[t] = l if (0..365).include?((t-Time.now) / 86400)
  end
end

ts.sort.each { |k,v| puts "#{k} #{v.chomp.ljust(6,'.')}" }
