(* "Each of" type (e.g., tuple), classes (Java), record) *)

type item = {
  description: string;
  price: float;
  in_stock: int;
  num_sold: int;
}

let item1: item = {
  description = "Expo dry erase marker";
  price = 2.74;
  in_stock = 10;
  num_sold = 45;
}

let item2: item = {
  in_stock = 5;
  num_sold=2;
  price=12.;
  description= "whatever";
}

let sold = item2.num_sold

let rec total_sold(xs: item list): int =
  if xs = [] then
    0
  else
    (List.hd xs).num_sold + total_sold(List.tl xs)

let tot_sold: int = total_sold [item1;item1;item2;item2]

(* "One of" type (e.g., option, variant) *)

type color = 
    |Red
    |Green
    |Blue
    |White
    |Black

let my_color: color = Blue

let get_rgb(c: color): int * int * int =
    if c = Red then
      (255, 0, 0)
    else if c = Green then
      (0,255,255)
    else
      (0,0,0)

let my_rgb: int*int*int = get_rgb my_color

let get_rgb(c: color):int*int*int = 
    match c with
    |Red -> (255,0,0)
    |Green -> (0,255,0)
    |Blue -> (0,0,255)
    |Black -> (0,0,0)
    |White -> (255,255,255)

let my_rgb: int*int*int = get_rgb my_color