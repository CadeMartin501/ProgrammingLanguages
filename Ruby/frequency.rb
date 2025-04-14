def clean_word(word)
  word.downcase
end

line = gets
while line
  line.split.each do |word|
    puts word
  end
  line = gets
end