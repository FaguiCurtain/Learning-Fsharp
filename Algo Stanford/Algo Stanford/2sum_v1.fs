///////////////// preparing the data ////////////////////

open System
open System.Collections.Generic
open System.IO

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\PA 6 - 2sum.txt"

let y = x |> Array.map int64

let dict = new Dictionary<int64,int64 list >()

let insert_or_update (x:int64) = 
    let y = (x / 10000L)
    if dict.ContainsKey y then dict.[y]<-x::dict.[y] else dict.Add(y,[x])

y|> Array.map (fun x ->insert_or_update x) |> ignore

let mutable temp = 0
let mutable ok = false
let mutable i = 0


let keylist = Seq.toList dict.Keys // ça doit être possible de faire cela en même temps que insert_or_update
let keypluslist = keylist |> List.filter (fun s -> s>= 0L)


let answer = [|for i in -10000..10000 do yield false|]

// cas 0 est particulier avec une vérif supplémentaire
//for x in dict.[0L] do
//    for y in dict.[0L] do
//        let t = x+y
//        if ( t>= -10000L && t<=10000L && (x<>y) ) then  answer.[10000 + int t]<- true
//
//for x in dict.[0L] do
//    for y in dict.[-1L] do
//        let t = x+y
//        if ( t>= -10000L && t<=10000L ) then  answer.[10000 + int t]<- true

let mutable count = 0 
for i in keypluslist do
    count <- count + 1
    if (count % 100 = 0) then printfn "count = %d" count
    if dict.ContainsKey i then
                               for x in dict.[i] do
                                   if dict.ContainsKey (-i) then
                                                               for y in dict.[-i] do
                                                                   let t = x+y
                                                                   if ( t>= -10000L && t<=10000L ) then  
                                                                                                      // if answer.[10000 + int t] = false then printfn "x= %A y=%A t=%A" x y t
                                                                                                       answer.[10000 + int t]<- true
                                                                                                         
                                   if dict.ContainsKey (-i-1L) then
                                                               for y in dict.[-i-1L] do
                                                                   let t = x+y
                                                                   if ( t>= -10000L && t<=10000L ) then  
                                                                                                   //    if answer.[10000 + int t] = false then printfn "x= %A y=%A t=%A" x y t 
                                                                                                       answer.[10000 + int t]<- true                                
                                   if dict.ContainsKey (-i+1L) then
                                                               for y in dict.[-i+1L] do
                                                                   let t = x+y
                                                                   if ( t>= -10000L && t<=10000L ) then  
                                                                                                    //   if answer.[10000 + int t] = false then printfn "x= %A y=%A t=%A" x y t 
                                                                                                       answer.[10000 + int t]<- true                                   

                                                                                                        
let len = Array.length answer
printfn "len= %A" len

let ans = answer |> Array.filter (fun s -> s=true) |> Array.length

printfn "answer = %d" ans

stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds
Console.ReadKey() |> ignore

