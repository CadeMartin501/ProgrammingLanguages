(* Question 9 *)

let rec replace_chosen f r xs= 
match xs with 
|x::xs' -> 
  if f x (* if true, it will replace *)
    then
      r::replace_chosen f r xs' 
    else 
      x::replace_chosen f r xs'
|[] -> []
let test1 = replace_chosen (fun a -> a mod 2 = 0) 42 [1;2;3;4]

(* Question 10 *)
let negatives_to_zeros xs = replace_chosen (fun a -> if a < 0 then true else false) 0 xs
let test2 = negatives_to_zeros [-2;-1;0;1;2]

(* Question 11 *)
let rec keep_while f xs =
  match xs with
  |x::xs' -> 
    if f x
      then
        x::keep_while f xs'
    else
      []
  |[] -> []
let test3 = keep_while (fun x -> x < 5) [1;2;3;6;4]

(* Question 12 *)
let rec larger l xs =
  List.filter (fun x -> x>l) xs
let test4 = larger 5 [6;3;7;4;8]