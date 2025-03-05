(* map and filter *)

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