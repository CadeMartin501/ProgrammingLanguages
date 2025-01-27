let rec factorial(n : int) : int = 
  if n=0 then 
    1
  else 
    n * factorial(n-1)

let rec add((a:int), (b:int)) : int =
  if a=0 then
    b
  else
    add(a-1, b+1)

    let addPair((pr1 : int * int), (pr2 : int * int)) : int * int =
(fst pr1 + fst pr2, snd pr1 + snd pr2) 

let sum = addPair((1,2), (3,4))

let swapPair(pair : int * int) : int * int =
    (snd pair, fst pair)

    let swapTest =swapPair(1,2)