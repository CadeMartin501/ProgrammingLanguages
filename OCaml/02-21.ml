let rec is_member (xs, key) =
  match xs with
  |x::xs' -> if x=key then true else is_member(xs', key)
  |[] -> false

  let result = is_member([1;2;3], 4)

  (* ^^^^ QUIZ *)

(* 
zip ([1; 2; 3], [4; 5; 6])
*)

(* Precondition: Both lists have the same number of elements. *)
(*
let rec zip(xs, ys) =
  match xs with
  |[] -> []
  |x::xs' -> 
*)

let rec zip pr =
  match pr with
  |([],_) -> []
  |(_,[]) -> []
  |(x::xs', y::ys') -> (x,y)::zip(xs',ys')

let results = zip ([1;2;3],[4;5;6])

let rec unzip xs =
  match xs with
  | [] -> ([],[])
  | x::xs' -> let temp = unzip xs' in 
      fst x::fst temp,snd x:: snd temp

let result2 = unzip([(1,4);(2,5);(3,6)])