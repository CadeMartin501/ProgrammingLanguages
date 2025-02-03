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

      let result: int = max([1;2;42;19;7;8])

      (*count_up_to(10) -> [1;2;3;4;5;6;7;8;9;10] *)
let count_up_to(n: int): int list =
  let rec build_range((lo: int), (hi: int)):
  int list =
    if lo > hi then
     []
    else
      lo :: build_range(lo+1, hi)
    in
    build_range(1,n)

    let rec max(xs: int list): int option = 
      if xs = [] then 
        None
    else if List.tl xs [] then
      List.hd xs
    else 