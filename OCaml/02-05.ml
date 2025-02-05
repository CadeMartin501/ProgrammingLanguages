let slope((p1: float * float), (p2: float * float)): float option = 
  if fst p2 -. fst p1 = 0. then
    None
  else 
    Some ((snd p2-.snd p1)/.(fst p2-.fst p1))
let my_slope: float option = slope((1., 6.), (3.,9.))

let rec max(xs: int list): int option =
  if xs = [] then
    None
  else if List.tl xs = [] then
    Some (List.hd xs)
  else 
    let max_so_far = max(List.tl xs) in
      if List.hd xs > Option.get(max_so_far) then
        Some(List.hd xs)
      else
        max_so_far

let top = max[1; 42; 99; 13; -5; 6]