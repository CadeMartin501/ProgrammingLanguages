(* Use these functions to extract parts of a date *)
let fst3 (x,_,_) = x (* gets the first element of a triple *)
let snd3 (_,x,_) = x (* gets the second element of a triple *)
let thd3 (_,_,x) = x (* gets the third element of a triple *)
(* DAY MONTH YEAR *)

(**
 * TODO: Complete the 11 function bindings described in the assignment.  For the first (2), 
 * we have given you the correct first line and an incorrect function body.
 *)

 (* 1 *)
 (* true if date1 is older than date2 *)
let is_older ((date1 : int * int * int), (date2 : int * int * int)): bool =
    if thd3 date1 < thd3 date2 then
        true
    else if thd3 date1 > thd3 date2 then 
        false
(*Comparing years^^^*)
    else if snd3 date1 < snd3 date2 then
        true
    else if snd3 date1 > snd3 date2 then
        false
(*Comparing months^^^*)
    else if fst3 date1 < fst3 date2 then
        true
    else
        false
(* Comparing days^^^ *)

(* 2 *)
let rec number_in_month ((dates : (int * int * int) list), (month : int)): int =
    if dates = [] then
        0
    else if thd3(List.hd dates) = month then
        1 + number_in_month (List.tl dates, month)
    else
        number_in_month (List.tl dates, month)

(* 3 *)
let rec number_in_months ((dates : (int * int * int) list), (months : int list)) : int =
    if months = [] then
        0
    else
        number_in_month(dates, List.hd months) + number_in_months(dates, List.tl months)

(* 4 *)
let rec dates_in_month ((dates : (int * int * int) list), (month: int)) : (int * int * int) list =
    if dates = [] then
        []
    else if snd3 (List.hd dates) = month then
        (List.hd dates) :: dates_in_month(List.tl dates, month)
    else
        dates_in_month(List.tl dates, month)

(* 5 *)
let rec dates_in_months ((dates : (int * int * int) list), (months : int list)) : (int * int * int) list =
    if months = [] then
        []
    else
        dates_in_month (dates, List.hd months) @ dates_in_months(dates, List.tl months)

(* 6 *)
let rec get_nth((strings: string list), (index : int)) : string =
    if index = 1 then
        List.hd strings
    else
        get_nth((List.tl strings),(index-1))

(* 7 *)
let string_of_date (date: int * int * int) =
    let months = ["January"; "February"; "March"; "April"; "May"; "June"; "July"; "August"; "September"; "October"; "November"; "December"] in
        get_nth(months, snd3 date)^"-"^string_of_int(fst3 date)^"-"^string_of_int(thd3 date)

(* 8 *)
let rec number_before_reaching_sum((sum: int), (nums : int list)) : int =
    let current_sum = List.hd nums in
        if current_sum >= sum then
            0
        else 
            1


(* 9 *)
let what_month (day: int) : int = 
    let days = [31; 59; 90; 120; 151; 181; 212; 243; 273; 304; 334; 365] in
        number_before_reaching_sum(day, days)


(* 10 *)
let month_range((day1: int), (day2 : int)) : int list =
[]

(* 11 *)
let cumulative_sum (nums : int list) : int list =
[]


let cumulative_sum_helper((nums : int list), (sum : int)) : int =
0