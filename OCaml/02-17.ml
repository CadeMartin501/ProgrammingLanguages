(* Let a boolean variable be true, false, or unknown. *)
type boolval = 
    | Known of bool
    | Unknown

(**************************)
(* Function for testing. *)
(**************************)
(* Generate all lists of length n. *)
(* int -> (expr list) list *)
let rec generate n =
    (* Given a list of expr lists, build a new list consisting of each of the
    current lists, each with Known true, Known false, and Unknown cons'd on. *)
    let rec add_to_each(xs) =
        match xs with
        | [] -> []
        | x::xs' -> (Known true::x) :: (Known false:: x) :: (Unknown::x) :: add_to_each xs'
    in
    match n with
    | 0 -> [[]]
    | _ -> add_to_each(generate(n-1))

let rec eval_and (xs: boolval list) : boolval=
    match xs with 
    (* Base cases *)
    |[] -> Unknown
    |Known(false) :: xs' -> Known(false)
    |_ :: Known(false) :: [] -> Known(false)
    |Unknown :: _:: [] -> Unknown
    |Known(true) :: x :: [] -> x
    (* End base cases *)
    |Known(true) :: xs' -> eval_and(xs')
    |Unknown::xs' -> if eval_and(xs')==Known(false) then Known(false) else Unknown

let result = eval_and([Unknown; Known true; Known false; Unknown;])