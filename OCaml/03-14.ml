(* map, filter, and fold *)

let rec map(f, xs) =
  match xs with
  |[] -> []
  |x::xs' -> f x :: map(f,xs')

let rec filter(f, xs) =
  match xs with
  |[] -> []
  |x::xs' -> if f x then x::filter(f, xs') else filter(f, xs')

let rec fold(f, acc, xs) =
  match xs with
  |[] -> acc
  |x::xs' -> fold(f, f(acc,x), xs')


let string_list=["red"; "green"; "blue"; "yellow"; "orange"]
let int_list = [1;2;3;4;5;6;7;8;9;10]
let exclaim = map((fun (k) -> k^"!"),string_list)
let root = map((fun (k) -> Float.sqrt(float_of_int k)), int_list)

let rec stars k =
  if k=1 then
    "*"
  else
    "*" ^ stars (k-1)

let stars_list = map(stars, int_list)

(* let rec is_prime k =
  let rec aux (k, factor) =
    if k < 2 then
      false
    else if factor > k then =
      true
else *)



(* let prime_list = filter(is_prime, int_list) *)