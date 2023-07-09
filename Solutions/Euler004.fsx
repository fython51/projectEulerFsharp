(*
    Problem 4: Largest Palindrome Product

    A palindromic number reads the same both ways. 
    The largest palindrome made from the product of two 2-digit numbers is 9009 = 91*99.
    Find the largest palindrome made from the product of two 3-digit numbers.
*)

// Define a function to determine whether a number is a palindrome
let palindrome (n: int) = 
    match string n |> Seq.toArray with
    | (nArray: char array) when nArray= (nArray |> Array.rev) -> true
    | _ -> false


// Iterate downwards, generate a list of palindromes
// The iteration needs to be i * (i-1), i * (i-2) ... (i-1)*(i-1), etc.
// Extract the largest one
let largestPalindrome (i: int) = 
    let createArray (i: int) = 
        Array.init i (fun (j: int) -> 
            Array.init i (fun (k: int) -> (k+1) * (i-j))
        )
    
    createArray i
    |> Array.concat
    |> Array.filter (fun (x: int) -> palindrome x)
    |> Array.max

largestPalindrome 999