(* map, filter, and fold *)

let rec map(f, xs) =
  match xs with
  |[] -> []
  |x::xs' -> f x :: map(f,xs')

let double n = n+n

let quick_list = [1;2;3;4;5]
let doubled = map(double, quick_list)

let rec filter(f, xs) =
  match xs with
  |[] -> []
  |x::xs' -> if f x then x::filter(f, xs') else filter(f, xs')

let even n = n mod 2 = 0
let evens = filter(even, quick_list)
let evens2 = filter((fun n -> n mod 2 = 0), quick_list)

let even_list = map(even, quick_list)
let rec fold(f, acc, xs) =
  match xs with
  |[] -> acc
  |x::xs' -> fold(f, f(acc,x), xs')

let sum(a, b) = a + b

let total = fold(sum, 0, quick_list)
let total = fold((fun (a,b) -> a+b), 0,quick_list)