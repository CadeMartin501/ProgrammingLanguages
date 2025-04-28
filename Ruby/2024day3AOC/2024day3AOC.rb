line = gets
sum = 0
while line
  regex = /mul\((\d{1,3}),(\d{1,3})\)/
  (line.scan regex).each{|a,b| sum+= a.to_i*b.to_i}
  line = gets
end

puts sum