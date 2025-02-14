(* Use these functions to extract parts of a date *)
let fst3 (x,_,_) = x (* gets the first element of a triple *)
let snd3 (_,x,_) = x (* gets the second element of a triple *)
let thd3 (_,_,x) = x (* gets the third element of a triple *)
(* DAY MONTH YEAR *)

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
(* Comparing days^^^*)

(* 2 *)
let rec number_in_month ((dates : (int * int * int) list), (month : int)): int =
    if dates = [] then
        0
    else if snd3(List.hd dates) = month then
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
    if sum <= 0 then
        0
    else
        1 + number_before_reaching_sum(sum-List.hd nums, List.tl nums)


(* 9 *)
let what_month (day: int) : int = 
    let days = [31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31] in
        number_before_reaching_sum(day, days)


(* 10 *)
let rec month_range((day1: int), (day2 : int)) : int list =
    if day1 > day2 then 
        []
    else
        let current_month = what_month day1 in
        if day1 = day2 then 
            [current_month]
        else
            let rest = month_range (day1 + 1, day2) in
            if List.hd rest = current_month then 
                rest
            else 
                current_month :: rest
        

(* 11 *)
let rec cumulative_sum (nums : int list) : int list =
    let rec helper ((reverse : int list), (sum : int), (nums : int list)) = 
        if nums = [] then 
            List.rev reverse
        else
            let x = List.hd nums in
            let xs = List.tl nums in
            helper(((sum + x) :: reverse), (sum + x), xs)
        in
        helper ([], 0, nums)

(* TEST CASES *)

(* 1. is_older *)
let test_is_older = [
    is_older ((1, 1, 2000), (2, 1, 2000)) = true; (* Older day *)
    is_older ((1, 1, 1999), (1, 1, 2000)) = true; (* Older year *)
    is_older ((1, 2, 2000), (1, 1, 2000)) = false (* Newer month *)
]

(* 2. number_in_month *)
let test_number_in_month = [
    number_in_month ([(1, 1, 2000); (2, 1, 2001); (3, 2, 2000)], 1) = 2;
    number_in_month ([(5, 3, 1999); (10, 3, 2020)], 3) = 2;
    number_in_month ([], 1) = 0
]

(* 3. number_in_months *)
let test_number_in_months = [
    number_in_months ([(1, 1, 2000); (2, 2, 2001); (3, 3, 2000)], [1; 3]) = 2;
    number_in_months ([(10, 5, 2022)], [5; 6]) = 1;
    number_in_months ([], [1; 2; 3]) = 0
]

(* 4. dates_in_month *)
let test_dates_in_month = [
    dates_in_month ([(1, 1, 2000); (2, 1, 2001); (3, 2, 2000)], 1) = [(1, 1, 2000); (2, 1, 2001)];
    dates_in_month ([], 5) = [];
    dates_in_month ([(1, 2, 2020); (2, 3, 2020)], 2) = [(1, 2, 2020)]
]

(* 5. dates_in_months *)
let test_dates_in_months = [
    dates_in_months ([(1, 1, 2000); (2, 2, 2001); (3, 3, 2000)], [1; 3]) = [(1, 1, 2000); (3, 3, 2000)];
    dates_in_months ([], [1; 2; 3]) = [];
    dates_in_months ([(5, 4, 2022); (7, 5, 2023)], [5]) = [(7, 5, 2023)]
]

(* 6. get_nth *)
let test_get_nth = [
    get_nth (["a"; "b"; "c"], 2) = "b";
    get_nth (["first"; "second"; "third"], 3) = "third";
    get_nth (["only"], 1) = "only"
]

(* 7. string_of_date *)
let test_string_of_date = [
    string_of_date (1, 1, 2000) = "January-1-2000";
    string_of_date (10, 6, 2023) = "June-10-2023";
    string_of_date (31, 12, 1999) = "December-31-1999"
]

(* 8. number_before_reaching_sum *)
let test_number_before_reaching_sum = [
    number_before_reaching_sum (10, [3; 3; 4; 5]) = 3;
    number_before_reaching_sum (5, [1; 1; 1; 1; 1]) = 5;
    number_before_reaching_sum (0, [2; 4; 6]) = 0;
]

(* 9. what_month *)
let test_what_month = [
    what_month 31 = 1;
    what_month 60 = 3;
    what_month 365 = 12;
]

(* 10. month_range *)
let test_month_range = [
    month_range (1, 10) = [1];
    month_range (32, 59) = [2];
    month_range (100, 105) = [4];
]

(* 11. cumulative_sum *)
let test_cumulative_sum = [
    cumulative_sum [1; 2; 3; 4] = [1; 3; 6; 10];
    cumulative_sum [5; 10; 15] = [5; 15; 30];
    cumulative_sum [] = []
]

(* Running all tests *)
let all_tests = test_is_older @ test_number_in_month @ test_number_in_months @ 
                test_dates_in_month @ test_dates_in_months @ test_get_nth @ 
                test_string_of_date @ test_number_before_reaching_sum @ 
                test_what_month @ test_month_range @ test_cumulative_sum


let _ = assert (List.for_all ((=) true) all_tests)
