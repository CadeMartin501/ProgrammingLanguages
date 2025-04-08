input = gets.split(', ')

direction = 0
locx = 0
locy = 0

input.each do |s|
  if s[0] == 'R' then
    direction = (direction + 1) % 4
  else 
    direction = (direction - 1) % 4
  end

  distance = s[1..].to_i
  case direction
  when 0
    locy += distance
  when 1
    locx += distance
  when 2
    locy -= distance
  when 3 
    locx -= distance
  end
end


puts locx.abs + locy.abs
