def clean_word(word)
  word.downcase
end


freqCount = Hash.new(0)
line = gets
while line
  line.split.each do |word|
    freqCount[word] += 1
  end
  line = gets
end

puts freqCount.sort_by {|_, b| -b}[..10]

