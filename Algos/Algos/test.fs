open System.IO

let test (A:int[]) =
    let L = A.Length

    match L with 
       | 0 -> "hello"
       | 1 -> "truc"
       | x -> "ben oui quoi"

let main argv = 
          printfn "Hello World"
          0