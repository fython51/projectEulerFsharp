(*
    Problem 3: Largest Prime Factor
    
    The prime factors of 13,195 are 5, 7, 13, and 29.
    What is the largest prime factor of the number 600851475143?
*)

let largestFactor (num: float) = 
    ([1 .. sqrt(num)] : float list)
    |> List.filter (fun (x : float) -> num%x=0)
    |> List.last

// largestFactor 40.
// Fix this