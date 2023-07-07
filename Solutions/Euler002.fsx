(*
Problem 2: Even Fibonacci Numbers

Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be: 
1, 2, 3, 5, 8, 13, 21, 34, 55, 89
By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
*)

// Define the active pattern
let (|Even|Odd|) number = if number%2=0 then Even else Odd

// Define a function to calculate the nth Fibonacci number
let rec fibonacci (num: int) = 
    match num with 
    | num when num <= 2 -> 1
    | _ -> fibonacci (num-1) + fibonacci (num-2)

// Define a function to calculate the sum of even Fibonacci numbers, starting from n to f(n) = 4,000,000
let rec fibonacciSum (num: int) = 
    let fSum = fibonacci num
    match fSum with
    | Even -> fSum + fibonacciSum (num+1)
    | n when n>4000000 -> 0
    | _ -> fibonacciSum (num+1)

fibonacciSum 1