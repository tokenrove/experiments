#!/usr/bin/env ruby

ts = {}
open('/usr/share/dict/words') do |f|
  f.each_line do |l|
    t = Time.at(l.downcase.tr('^a-z','').to_i(36))
    ts[t] = l if (0..365).include?((t-Time.now) / 86400)
  end
end

ts.sort.each { |k,v| puts "#{k} #{v}" }
