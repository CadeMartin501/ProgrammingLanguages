(* QUIZ START *)
(* type expr = 
  |Constant of int
  |Negate of expr
  |Add of expr * expr
  |Mult of expr * expr

let rec num_adds e = 
  match e with
  | Constant _ -> 0
  |Negate e1 -> num_adds(e1)
  |Add(e1, e2) -> 1 + num_adds(e1) + num_adds(e2) 
  |Mult(e1, e2) -> num_adds(e1) + num_adds(e2)

  let result = num_adds(Add(Constant 9, Add(Negate(Constant 7),Constant 4))) *)
  (* QUIZ END *)

type expr = 
  |Constant of int
  |Negate of expr
  |Add of expr * expr
  |Mult of expr * expr

type my_int_list =
  |Empty
  |Cons of int * my_int_list

let my_list = Cons(1, Cons(2, Cons(3, Cons(4, Empty))))

let rec list_sum xs =
  match xs with
  |Empty -> 0
  |Cons(x, xs') -> x + list_sum xs'

let result = list_sum(my_list)

let rec append (xs, ys) =
  match xs with 
  |Empty -> ys
  |Cons(x, xs') -> Cons(x, append(xs', ys))

let l = append(my_list, my_list)

let rec list_sum(xs: int list) : int =
  match xs with 
  |[] -> 0
  |x::xs' -> x + list_sum(xs') (*if you get a list, it binds the head(x) and tail(xs'), so you can use them later*)
  
let rec append(xs, ys) =
  match xs with
    |[] -> ys
    |x::xs' -> x :: append(xs', ys)