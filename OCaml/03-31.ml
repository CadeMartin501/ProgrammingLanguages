let rec filter(f, xs) =
match xs with 
| [] -> []
| x::xs' -> if f x then x::filter (f, xs') else filter (f, xs')

let my_list = [1;2;3;4;5;6;7]
let even x = x mod 2 = 0
let my_evens = filter(even, my_list)

let my_evens2 = filter((fun x -> x mod 2 = 0), my_list)

let get_evens xs = filter((fun x-> x mod 2 = 0), xs)

let my_evens3 = get_evens my_list

let inc = fun x -> x + 1
let inc2 x = x + 1 (* same thing as the one above. *)

let three = inc 2
let four = inc2 3

let five = (fun x -> x + 1) 4

let sorted(a,b,c) = a<=b && b<=c

let is_sorted = sorted(1,2,3)

let sorted = fun a -> (fun b -> (fun c -> (a<=b && b<=c)))

let is_sorted = (((sorted 1) 2) 3)
let is_sorted = sorted 1 2 3

let nonnegative = sorted 0 0

let get_evens2 = List.filter (fun x -> x mod 2 = 0)

let my_evens2 = get_evens2 my_list