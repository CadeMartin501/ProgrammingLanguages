let rec filter(f, xs) =
  match xs with
  |[] -> []
  |x::xs' -> if f x then x::filter(f, xs') else filter(f, xs')

let rec fold(f, acc, xs) =
  match xs with
  |[] -> acc
  |x::xs' -> fold(f, f(acc,x), xs')

let allGreaterThanSeven xs =
  filter((fun x ->  x > 7), xs)

let g7 = allGreaterThanSeven([1;29;29;18;17;4;-19;100])

let allGreaterThan(xs, n) =
filter((fun x -> x > n), xs)

let g18 = allGreaterThan([1;29;29;18;17;4;-19;100],18)

let allShorterThan(xs, s) =
  filter((fun x -> String.length(x) < String.length(s)), xs)

let allShorterThan2(xs, s) =
  let len = String.length(s) in
    filter((fun x -> String.length(x) < len), xs)

let countInRange(b, e, xs) =
  fold((fun(acc, x) -> if b<=x && x<=e then acc+1 else acc),0,xs)

let areNoneLonger(xs, s) =
  let len = String.length(s) in
    fold((fun (acc, x) -> if String.length x > len then false else true), true, xs)

let allAreShorter(xs, s) =
  let len = String.length s in
  fold((fun (acc, x) -> acc && String.length x <len), true, xs)

(* Type signature: int * int -> int *)
let sum1(a,b) = a + b

(* Type signature: int -> (int -> int) *)
let sum2 a = fun b -> a + b
let x = sum2 42
let y = (sum2 42) 9