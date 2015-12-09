// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
let phi = (1.0+sqrt 5.0)/2.0
open System

let myfun x = phi * x 



[<EntryPoint>]

let main argv = 
    Console.WriteLine("Enter the length of the array: ")
    let str =  Console.ReadLine()
    let length = int str

    let inputlist =
        [for k = 0 to (length-1) do
            let str =  Console.ReadLine()
            yield float str ]
    
    let outputlist = // returns a list of tuples [pairs] ( input,myfun(input) ) 
        [for k = 0 to (length-1) do
            yield (inputlist.[k],myfun (float inputlist.[k]))]

    let printoutput =
        for k = 0 to (length-1) do
            printfn "%f %f" <||outputlist.[k]
        0 //
    printoutput
    Console.ReadKey()
    0 // return an integer exit code
    //