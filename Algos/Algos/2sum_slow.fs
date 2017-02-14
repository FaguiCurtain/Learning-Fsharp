///////////////// preparing the data ////////////////////

open System
open System.Collections.Generic
open System.IO

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\PA 6 - 2sum.txt"

let y = x |> Array.map int64

let dict = new Dictionary<int64,int >()

let insert_or_update x = 
    if dict.ContainsKey x then dict.[x]<-dict.[x]+1 else dict.Add(x,1)

y|> Array.map (fun x ->insert_or_update x) |> ignore

let mutable temp = 0
let mutable ok = false
let mutable i = 0


let keylist = Seq.toList dict.Keys // ça doit être possible de faire cela en même temps que insert_or_update

let answer = [|for i in -10000..10000 do yield false|]


let rec onestep (sum:int64) temp list = 
    let mutable temp1 = 0

    if (temp>=2) then 
                      printfn "true"
                      answer.[10000+int sum] <- true 
                      [] 
                 else
                      match list with 
                        | []   -> []
                        | h::t -> if dict.ContainsKey h then if dict.ContainsKey (sum-h) 
                                                                then let c = dict.[sum-h]
                                                                     temp1 <- temp + c
                                                                     // if temp>=2 then answer.[t] <- true                         
                                                                else ()
                                                        else ()
                                  onestep sum temp1 t

for s in -9967..10000 do
    printfn "s=%d" s
    temp <- 0
    onestep (int64 s) temp keylist |> ignore

let ans = answer |> Array.filter (fun s -> s=true) |> Array.length

printfn "answer = %d" ans

stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds
Console.ReadKey() |> ignore
          

