input = gets.split(', ')

direction = 0
locx = 0
locy = 0
locations = Set.new

input.each do |s|
  if s[0] == 'R' then
    direction = (direction + 1) % 4
  else 
    direction = (direction - 1) % 4
  end

  distance = s[1..].to_i
  case direction
  when 0
    distance.times do
      locy += 1
      if locations.member? [locx, locy] then
        puts locx.abs + locy.abs
        exit 0
      else
        locations << [locx, locy]
      end
    end
    # locy += distance
  when 1
    distance.times do
      locx += 1
      if locations.member? [locx, locy] then
        puts locx.abs + locy.abs
        exit 0
      else
        locations << [locx, locy]
      end
    end
    # locx += distance
  when 2
    distance.times do
      locy -= 1
      if locations.member? [locx, locy] then
        puts locx.abs + locy.abs
        exit 0
      else
        locations << [locx, locy]
      end
    end
    # locy -= distance
  when 3 
    distance.times do
      locx -= 1
      if locations.member? [locx, locy] then
        puts locx.abs + locy.abs
        exit 0
      else
        locations << [locx, locy]
      end
    end
    # locx -= distance
  end
end


puts locx.abs + locy.abs
