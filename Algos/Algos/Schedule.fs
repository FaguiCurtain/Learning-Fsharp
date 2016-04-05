open System
open System.Collections.Generic
open System.IO

open FSharp.Core

open FSharpx.Collections

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

///////////////// preparing the data /////////////////

let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\PA1 - jobs.txt"
let nbjobs = int x.[0]

let split (text:string)=
    text.Split [|'\t';' '|]

let splitIntoKeyValue (A: 'T[]) =  
    (int64 A.[0], int64 A.[1])

let parseLine (line:string)=
    line
    |> split
    |> Array.filter (fun s -> not(s=""))
  //  |> Array.map (fun s-> (int s))
    |> splitIntoKeyValue

let list = x |> Array.tail |> Array.map parseLine |> Array.toList

///////////////// applying the greedy algo ////////////////
let list1 = list |> List.map (fun (x,y) -> (float (x-y),x,y)) |> List.sort |> List.rev

let list2 = list |> List.map (fun (x,y) -> ( (float x)/(float y),x,y)) |> List.sort |> List.rev

let count = List.fold (fun (acc,timeacc) (x,y,z)-> let t = timeacc + int64 z
                                                   (acc + y*t , t)
                       ) (0L,0L)

let count1 = count list1 // priorizing highest weight - length
let count2 = count list2 // priorizing highest weight / length

//val count1 : int64 * int64 = (69119377652L, 510289L)
//val count2 : int64 * int64 = (67311454237L, 510289L)
