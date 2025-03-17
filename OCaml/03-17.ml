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

let partition(xs, pivot) =
  (filter((fun x-> x<pivot), xs),
  filter((fun x -> x>=pivot), xs))

let result = partition([26; 4; 16; 38; 10; 8; 21; 12], 16)

let partition2 (xs, pivot) =
  fold((fun ((lt,gt), x) ->
    if x < pivot then (x::lt,gt)
    else if x > pivot then (lt, x::gt)
    else (lt, gt)
    ),
  ([], []),
  xs)


let result = partition([26; 4; 16; 38; 10; 8; 21; 12], 16)