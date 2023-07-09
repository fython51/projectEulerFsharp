(*
    Exercise 1
    Write a tail-recursive function that counts down from a given number to zero. 
    The function should print each number as it counts down.
*)

// Build the function
let tailCountdown (n: int) = 
    let rec loop (n: int) = 
        match n with
        | (num: int) when num < 0 -> ()   // Do nothing
        | _ -> printfn "%d" n; loop (n-1) // Print n and repeat the function with n-1 
    loop n

// Test it
tailCountdown 5


(*
    Exercise 2
    Write a tail-recursive function that sums all numbers from a given number down to zero. 
    For example, if the input is 5, the output should be 15 (because 5 + 4 + 3 + 2 + 1 = 15).
    N.B. don't just make a calculator for n(n+1)/2, this would not be recursion.
*)

// Build the function
let tailTriangularCalculator n =
    let rec loop (n: int) (acc: int) = 
        match n with
        | num when num <= 0 -> acc
        | _ -> loop (n - 1) (acc + n)
    loop n 0
    
// Test the function
tailTriangularCalculator 5


(*
    Exercise 3
    Write a recursive function to calculate the factorial of a number. The factorial of a number n is the product of all positive integers less than or equal to n. 
    For example, the factorial of 5 (denoted as 5!) is 1*2*3*4*5 = 120.
*)

// Build the function
let tailFactorial (n: int) = 
    let rec loop (n: int) (acc: int) = 
        match n with
        | (num: int) when num <= 1 -> acc
        | _ -> loop (n-1) (acc*n)
    loop n 1

tailFactorial 5


(*
    Exercise 4
    Write a tail-recursive function to generate the Fibonacci sequence. 
    The Fibonacci sequence is a series of numbers in which each number is the sum of the two preceding ones, usually starting with 0 and 1.
*)

// Build the function
let tailFibonacciSeq n =
    let rec loop n a b =
        match n with
        | 0 -> printfn "%d" a
        | _ -> 
            printfn "%d" a
            loop (n - 1) b (a + b)
    loop n 0 1

// Test the function
tailFibonacciSeq 10