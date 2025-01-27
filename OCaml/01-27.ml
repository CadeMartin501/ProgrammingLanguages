let pr: int * int = (1,2)

let equal_pair((a: int*int), (b : int * int)) : bool =
  fst a = fst b && snd a = snd b

let lst = [1; 2; 3; 4]

let first_item = List.hd lst
let rest_items = List.tl lst

let second = List.hd(List.tl lst)

let rec length(xs : int list) : int =
  if xs = [] then
    0
  else
    1 + length(List.tl xs)