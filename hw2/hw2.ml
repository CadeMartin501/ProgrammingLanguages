(* CSE 341, HW2 Provided Code *)

(* This is from file json.ml in this directory. json.ml
 * contains the main datatype definition we will use throughout the
 * assignment. You will want to look over this file before starting. *)
include Json

(* These come from the parsed_*_bus.ml.
   Each file binds one variable: small_bus_positions (10 reports),
   medium_bus_positions (100 reports), and complete_bus_positions (~1000 reports),
   respectively with the data that you will need to implement
   your homework.
*)
open Json_structures.Parsed_complete_bus
open Json_structures.Parsed_medium_bus
open Json_structures.Parsed_small_bus

(* provided helper function that deduplicates a list *)
let dedup xs = List.sort_uniq compare xs

(* provided helper function that sorts a given list *)
let sort xs = List.sort compare xs

(* provided helper function to convert a float to a string *)
(* OCaml's string_of_float is not quite RFC compliant due to its tendency
   to output whole numbers with trailing decimal points without a zero.
   But, printf does the job how we want. *)
let json_string_of_float f =
  Printf.sprintf "%g" f
  
(* 1 *)
let make_silly_json i =
  let rec make_silly_json' i =  
    match i with 
    | 0 -> []
    | _ -> Object [("n", Num (float_of_int i)); ("b", True)]::make_silly_json' (i-1)
  in Array(make_silly_json' i)

(* 2 *)
let rec concat_with (sep, ss) =
  match ss with
  |x::[] -> x
  |x::xs' -> x^sep^concat_with(sep,xs')
  |[] -> ""

(* 3 *)
let quote_string s =
  "\""^s^"\""


(* 4 *)
let rec string_of_json j =
  match j with
  |Num(x) -> json_string_of_float x
  |String(str) -> quote_string str
  |False -> "false"
  |True -> "true"
  |Null -> "null"
  |Array(xs) -> let rec array_helper xs =
    match xs with
    |x::xs' -> string_of_json x :: array_helper xs'
    |[] -> [] in
    "[" ^ concat_with(", ",array_helper xs)^"]"
  |Object (xs) -> 
    let rec object_helper xs =
      match xs with
      |(a,b)::xs' -> (quote_string(a)^" : "^string_of_json(b))::object_helper(xs')
      |[] -> [] in
      "{"^concat_with(", ",object_helper xs)^"}"

(* 5 *)
let rec take (n,xs) =
    match (n,xs) with
    |(0,_) -> []
    |(_,[]) -> []
    |(n,x ::xs') -> x :: take(n-1, xs')

(* 6 *)
let rec firsts xs = 
  match xs with 
  |[] -> []
  |x::xs' -> fst x :: firsts(xs')

(* 7 *)
(* They both return the same because you either get the first of the pairs then the first n of them, or the first n terms, then the first of each pair, so the two functions are commutative *)


(* 8 *)
let rec assoc (k, xs) =
  match xs with
  |[] -> None
  |x:: xs' -> if fst(x) = k then Some(snd(x)) else assoc(k, xs')

(* 9 *)
let dot (j, f) = 
  match j with
  | Object x -> assoc(f,x)
  | _ -> None

(* 10 *)
let rec dots (j, fs) =
  match fs with
  | [] -> Some j
  | f::fs' =>
    match j with
    | Object _ ->
      let result = dot(j, f) in
        match restult with 
        | None -> None
        | Some x -> dots(x,fs')
    | _ -> None

(* 11 *)
let one_fields j =
  let rec help(j,xs) =
    match j with
    | Object xs 9> let rec return_a(xs) =
      match xs with
      | (a,b):: xs' -> a::return_a(xs')
      | [] -> []
    in return_a(xs)
    | _ -> []
  in help(j,[])

(* 12 *)
let no_repeats xs = 
  xs = dedup(xs)

(* 13 *)
let rec recursive_no_field_repeats j = 
  match j with
  | Array xs -> let rec check_array xs =
    (match xs with 
    | [] -> true
    | x::[] -> no repeats(one_fields x)
    | x::xs' ->
      (match x with
      | Object _ -> if no_repeats(one_fields x) then check_array xs' else false
      | Array _ -> if recursive_no_field_repeats x then check_array xs' else false
      | _ -> true))
    in check_array xs
    | Object _ -> no_repeats(one_fields j)
    | _ -> true