(*
    Problem 3: Largest Prime Factor
    
    The prime factors of 13,195 are 5, 7, 13, and 29.
    What is the largest prime factor of the number 600,851,475,143?
*)

// Define a function to find the factors of a number
let factors (n: float) = 
    [1. .. sqrt(n)]
    |> List.filter (fun (x: float) -> n%x=0)

// Define an active pattern to determine whether a number is prime or not
let (|Prime|_|) (n: float) = 
    match factors n with
    | [1.] -> Some(n)
    | _ -> None

// Define a function to filter the prime numbers out of the factors of a number
let primeFactors (n: float) =
    factors n
    |> List.filter (fun x -> match x with | Prime _ -> true | _ -> false)
    |> List.max

primeFactors 600851475143.