let sum_evens xs =
  let rec sum_evens' (xs, acc) =
  match xs with 
  |[] -> acc
  |x::xs' -> 
    if x mod 2 = 0 then
      sum_evens' (xs', acc+x)
    else
      sum_evens'(xs', acc)
    
  in
    sum_evens'(xs,0)

let my_list=[1;2;3;4;5;6]

type miniHTML =
 |Text of string
 |Image of string * int * int
 |BulletList of miniHTML list

 let rec numPixels element =
  let helper (element, acc) =
  match element with
  |Text _ -> 0
  |Image (e1,e2,e3) -> (e2*e3)+acc
  |BulletList (x::xs') -> numPixels(x)+acc +numPixels(BulletList xs')
  |BulletList ([]) -> 0

  in
  helper(element,0)



let digit_helper x =
  if x<10 then
    [None]
  else
    [Some(x/10 mod 10)]


let rec tens_digit_option (xs: int list): int option list =
 match xs with
  |[] -> []
  |x::[] -> digit_helper x
  |x::xs' -> digit_helper x @ tens_digit_option xs'

let lst =[123;4567;8;910]
let result = tens_digit_option lst