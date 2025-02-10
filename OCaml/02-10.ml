type color = 
    |Red
    |Green
    |Blue
    |White
    |Black

let my_color: color = Blue

let get_rgb c = 
    match c with
    |Red -> (255,0,0)
    |Green -> (0,255,0)
    |Blue -> (0,0,255)
    |Black -> (0,0,0)
    |White -> (255,255,255)

let my_rgb = get_rgb my_color

type silly = 
  | A of int*bool*(string list)
  | Foo of string
  |Pizza

let my_silly1: silly = Pizza
let my_silly = Foo "hello"
let my_silly3 = A(25, true, ["one"; "two"; "three"])

(* 
type my_int_option =
  |Some int
  |None
 *)

 type expr = 
  |Constant of int
  |Negate of expr
  |Add of expr*expr
  |Mult of expr*expr

let forty_two: expr = Constant(42)
let negative_forty_two = Negate(Constant(42))
let negative_forty_two = Negate(forty_two)
let negative_twenty_eight = Add(Constant 12, Negate(Constant 40))
let twenty_two = Add(Mult(Constant 3, Constant 5), Constant(7))
let thirty_six = Mult(Constant 3, Add(Constant 5, Constant 7))

(* expr -> int *)
let rec evaluate e =
  match e with
  |Constant value -> value
  |Negate expression -> -evaluate(expression)
  |Add(e1,e2) -> evaluate e1 + evaluate e2
  |Mult(e1,e2) -> evaluate e1 * evaluate e2

let rec count_constants e = 
  match e with
  | Constant value -> 1
  | Negate expression -> count_constants expression
  | Add(e1, e2) -> count_constants e1 + count_constants e2
  | Mult(e1, e2) -> count_constants e1 + count_constants e2

let one = count_constants forty_two
let three = count_constants twenty_two

let twenty_two = evaluate twenty_two