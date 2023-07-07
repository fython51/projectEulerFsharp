(*
Problem 1: Multiples of 3 or 5

If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6, and 9. The sum of these multiples is 23.
Find the sum of all the multiples of 3 or 5 below 1,000.
*)

// Set up an active pattern
let (|MultOfThree|_|) (num: int) = 
    match num%3=0 with
    | true -> Some(num)
    | _ -> None

let (|MultOfFive|_|) (num: int) = 
    match num%5=0 with
    | true -> Some(num)
    | _ -> None

// Define a recursive function to calculate the sum
let rec sumMults (num: int) = 
    match num with 
    | MultOfThree n -> n + sumMults (num - 1)
    | MultOfFive n -> n + sumMults (num - 1)
    | n when n > 0 -> sumMults (num - 1)
    | _ -> 0

// Make the calculation
sumMults 999