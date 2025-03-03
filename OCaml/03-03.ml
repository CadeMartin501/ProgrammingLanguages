let rec double_n_times(n, x) =
  if n=0 then
    x
  else
    double_n_times(n-1,x*2)


let two_to_the_tenth = double_n_times(10,1)

let rec increment_n_times (n, x) =
  if n=0 then
    x
  else increment_n_times(n-1,x+1)

let ten = increment_n_times(4,6)

let rec tail_n_times(n, x) =
  if n=0 then
    x
  else
    tail_n_times(n-1,List.tl x)


let short_list = tail_n_times(4, [1;2;3;4;5;6;7;8;9])

let rec n_times(n, x, f) = 
  if n=0 then
    x
  else
    n-times(n-1, f x)