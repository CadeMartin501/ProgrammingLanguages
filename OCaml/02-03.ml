(* Precondition: There is at least one item in the list. *)
let rec max(xs: int list): int = 
  if List.tl xs = [] then
      List.hd xs
  else if List.hd xs > max(List.tl xs) then
      List.hd xs
  else
      max(List.tl xs)

let result: int = max [1; 2; 42; 19; 7; 8]

(* count_up_to(10) -> [1; 2; 3; 4; 5; 6;7 ;8; 9; 10] *)
let count_up_to(n: int): int list =
  let rec build_range((lo: int), (hi: int)): int list =
      if lo > hi then
          []
      else
          lo :: build_range(lo+1, hi)
  in
  build_range(1, n)

(* let result: int list = build_range(1, 10) *)
let result = count_up_to(10)

let count_up_to(hi: int): int list =
  let rec build_range(lo: int): int list =
      if lo > hi then
          []
      else
          lo :: build_range(lo+1)
  in
  build_range(1)

(* let result: int list = build_range(1, 10) *)
let result = count_up_to(10)

(* Precondition: There is at least one item in the list. *)
let rec max(xs: int list): int =
  if List.tl xs = [] then
      List.hd xs
  else
      let max_so_far = max(List.tl xs) in        
          if List.hd xs > max_so_far then
              List.hd xs
          else
              max_so_far

let result: int = max(count_up_to(100))

(* Precondition: There is at least one item in the list. *)
let rec max(xs: int list): int =
  if List.tl xs = [] then
      List.hd xs
  else
      let max_so_far = max(List.tl xs) in
          let first_item: int = List.hd xs in
              if first_item > max_so_far then
                  first_item
              else
                  max_so_far

let result: int = max(count_up_to(100))

(* let rec max(xs: int list): int option =
  if xs = [] then
      None
  else if List.tl xs = [] then
      Some(List.hd xs)
  else ... to be continued... *)