let rec f xs =
  match xs with
  |[] -> []
  |x::xs' -> x @ f(xs')

let rec j xs =
  let rec j' (xs, acc) =
    match xs with 
    |[] -> acc
    |x::xs' -> j'(xs',acc@x)
  in
    j'(xs,[])

let my_list=[[1;2;3];[4;5;6];[7;8;9]]
let result = j my_list

(* Precondition: there is at least one int in xs. *)

(* BADDDDDD *)
let rec max xs =
  match xs with
  |[] -> -1
  |x::[] -> x
  |x::xs'->
    let max_ahead = max xs' in
    if x> max_ahead then
      x
    else
      max_ahead

let list=[1;2;3;4;5]
let result = max list

(* GOOOODDDDDD *)
let rec max xs =
  let rec max'(xs,curr) =
    match xs with
    |[] -> curr
    |x::xs' -> max'(xs', if curr < x then x else curr)

  in max'(List.tl xs, List.hd xs)


let list=[1;2;3;4;5]
let result = max list