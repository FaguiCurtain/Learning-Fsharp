open System

open FSharp.Charting

let ch = Chart.Line [ for x in 0 .. 10 -> x, x*x ]
ch.ShowChart()
    

[<EntryPoint>]
let main args =
    printfn "Arguments passed to function : %A" args
    printfn "hello world"
    0

