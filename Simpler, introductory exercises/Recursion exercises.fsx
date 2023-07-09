(*
    Exercise 1
    Write a recursive function that counts down from a given number to zero. 
    The function should print each number as it counts down.
*)

// Build the function
let rec countdown (n: int) = 
    match n with
    | (num: int) when num < 0 -> ()        // Do nothing
    | _ -> 
        printfn "%d" n  // Print n 
        countdown (n-1) // Repeat the function with n-1 

// Test it
countdown 5


(*
    Exercise 2
    Write a recursive function that sums all numbers from a given number down to zero. 
    For example, if the input is 5, the output should be 15 (because 5 + 4 + 3 + 2 + 1 = 15).
    N.B. don't just make a calculator for n(n+1)/2, this would not be recursion.
*)

// Build the function
let rec triangularCalculator (n: int) = 
    match n with
    | (num: int) when num <= 0 -> 0
    | _ -> n + triangularCalculator (n-1)

// Test the function
triangularCalculator 5


(*
    Exercise 3
    Write a recursive function to calculate the factorial of a number. The factorial of a number n is the product of all positive integers less than or equal to n. 
    For example, the factorial of 5 (denoted as 5!) is 1*2*3*4*5 = 120.
*)

// Build the function
let rec factorial (n: int) = 
    match n with
    | (num: int) when num <= 1 -> 1
    | _ -> n * factorial (n-1)

factorial 5


(*
    Exercise 4
    Write a recursive function to generate the Fibonacci sequence. 
    The Fibonacci sequence is a series of numbers in which each number is the sum of the two preceding ones, usually starting with 0 and 1.
*)

// Build the function
let rec fibonacciSeq n =
    match n with
    | 0 | 1 -> printfn "%d" n; n
    | _ -> 
        let prev = fibonacciSeq (n - 1)
        let result = prev + fibonacciSeq (n - 2)
        printfn "%d" result
        result


// Test the function
fibonacciSeq 10