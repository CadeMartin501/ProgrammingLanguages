let my_list: int list = [1; 2; 3; 4; 5]

let rec list_sum(xs: int list): int = 
  if xs = [] then 
    0
  else
    List.hd xs + list_sum(List.tl xs)

let result = list_sum my_list

(* Precondition: n is non-negative. *)
let rec countdown(n: int): int list = 
  if n = 0 then
    []
  else
    n :: countdown(n-1) (*cons n onto the coundown of the rest of the list *)

let result = countdown 10

(* Precondition: xs has at least one int. *)
let rec last(xs: int list) : int = 
  if List.tl xs = [] then
    List.hd xs
  else 
    last(List.tl xs)

let result = last my_list

let rec contains((xs: int list),(key : int)) : bool = 
  if xs = [] then
    false

  else if List.hd xs = key then
    true
  else 
    contains(List.tl xs, key)

let result = contains((my_list), 6)

(* Precondition: There are at least index items in the list. *)
let rec get_at_index((xs: int list), (index: int)) : int =
  if index = 0 then
    List.hd xs
  else 
    get_at_index((List.tl xs), index-1)

let result = get_at_index((my_list), 3) (*4*)
let result = get_at_index((countdown 10, 3)) (*6*)

let rec append((xs: int list), (ys: int list)): int list = 
  if xs = [] then 
    ys
  else
    List.hd xs ::append(List.tl xs, ys)

let result = append([1; 2; 3], [4; 5; 6])