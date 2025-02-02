let i : int = 42
let a : int = 99

let add((a : int), (b : int)) : int =
    a + b

let z = add(a, 123)

let rec pow((base : int), (exponent : int)) : int =
    if exponent = 0 then
        1
    else
        base * pow(base, exponent-1)

let result : int = pow(2, 10)