type expr = 
  |Constant of int
  |Negate of expr
  |Add of expr * expr
  |Mult of expr * expr

(*-19*)
let a = Add(Constant 9, Mult(Negate(Constant 7),Constant 4))

let rec evaluate e = 
  match e with 
  |Constant value -> value
  |Negate e1 -> -evaluate e1
  |Add(e1,e2) -> evaluate e1 + evaluate e2
  |Mult(e1,e2) -> evaluate e1 * evaluate e2

let result = evaluate a

let rec max_constant e =
  let max(a, b) = if a>b then a else b in
  match e with
  |Constant value -> value
  |Negate e1 -> max_constant e1
  |Add(e1, e2) -> max(max_constant e1, max_constant e2)
  |Mult(e1, e2) -> max(max_constant e1, max_constant e2)

  let result = max_constant a

  let rec has_constant_not_under_add e =
    match e with
    |Constant _ -> true
    |Negate e1 -> has_constant_not_under_add e1
    |Add(_, _) -> false
    |Mult(e1, e2) -> has_constant_not_under_add e1 || has_constant_not_under_add e2

let result = has_constant_not_under_add a
