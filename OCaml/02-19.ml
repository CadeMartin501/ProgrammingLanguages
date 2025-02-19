type boolval = 
    | Known of bool
    | Unknown


(* My previous try *)
(* let rec eval_and (xs: boolval list) : boolval=
    match xs with
    |[] -> Unknown (* This shouldn't happen. *)
    |_::[] -> Unknown (* This shouldn't happen. *)
    (* Base cases *)
    |Known(false) :: xs' -> Known(false)
    |_ :: Known(false) :: [] -> Known(false)
    |Unknown :: Unknown :: [] -> Unknown
    |Known(true) :: x :: [] -> x
    (* End base cases *)
    |Known(true) :: xs' -> eval_and(xs')
    |Unknown::xs' -> if eval_and(xs')==Known(false) then Known(false) else Unknown *)


(* In class *)
let rec eval_and xs = 
  match xs with
  |[] -> Unknown (* Should not happen. *)
  |_::[] -> Unknown (* Shoudl not happen *)
  |Known false::_ -> Known false
  |_::Known false::[] -> Known false
  |Known true::Known true::[] -> Known true
  |_::_::[] -> Unknown
  |Unknown::xs' -> if eval_and xs' = Known false then Known false else Unknown
  |Known true::xs' -> eval_and xs'

let result = eval_and([Known true; Known true; Known true; Known false;])

let sum_triple(triple) =
  match triple with
  |(a,b,c) -> a + b + c

  let my_sum = sum_triple (1,2,3)

  let sum_triple(triple) =
  let (a,b,c) = triple in a + b + c

  let my_sum = sum_triple (1,2,3)

  let sum_triple(a,b,c) =
    a + b + c

let my_sum = sum_triple(1,2,3)