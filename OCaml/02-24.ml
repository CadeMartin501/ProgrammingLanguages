let rec factorialbad n =
  match n with
  | 0 -> 1
  | _ -> n * factorialbad(n-1)

let factorial n =
  let rec factorial'(n, acc) =
      match n with
      | 0 -> acc
      | _ -> factorial'(n-1, acc*n)
  in
      factorial'(n, 1)

let rec list_sum xs =
  match xs with
  | [] -> 0
  | x::xs' -> x + list_sum xs'

let list_sum xs =
  let rec list_sum'(xs, acc) =
      match xs with
      | [] -> acc
      | x::xs' -> list_sum'(xs', acc+x)
  in
      list_sum'(xs, 0)

let rec reverse xs =
  match xs with
  | [] -> []
  | x::xs' -> reverse xs'@[x]

let rec append(xs, ys) =
  match xs with
  | [] -> ys
  | x::xs' -> x::append(xs', ys)