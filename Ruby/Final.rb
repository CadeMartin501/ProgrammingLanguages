# Question 5
a = 3.0
b = 3.0
puts a.__id__
puts b.__id__
# both return '18014398509481986' which means they both return the same id, so floats are refs

# Question 6
c = 'hello'.__id__
d = 'hello'.__id__
puts c
puts d
# puts c returns 2429430400, while puts d returns 2429430380, so they are not refs since they are the same string but different id's

# Question 7
# List.map List.length [[10]; [20]; [30]] <- OCaml
e =[[10], [20], [30]]
puts e.map {|f| f.length}
# List.map List.length [[10]; [20,21,22]; [30,31,32,33,34]] <- OCaml
g = [[10], [20,21,22], [30,31,32,33,34]]
puts g.map {|h| h.length}

# Question 8
input = '710 North Park Dr.'
inputRegexp = /\A(\d+) (.+) (St\.|Dr\.|Ave\.)/ #Could be more street types
puts (input.match(inputRegexp)&.captures)