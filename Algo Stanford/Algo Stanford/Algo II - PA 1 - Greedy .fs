///////////////// preparing the data ////////////////////

open System
open System.Collections.Generic
open System.IO

open FSharp.Core

open FSharpx.Collections

let stopWatch = System.Diagnostics.Stopwatch.StartNew()


let x = File.ReadAllLines "/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/Algo Stanford/PA1 - jobs.txt"

// format of the files
// [number_of_jobs]
// [job_1_weight] [job_1_length]
// [job_2_weight] [job_2_length]

let split (text:string)=
    text.Split [|'\t';' '|]

let splitIntoValues (A: 'T[]) =  
    (int64 A.[0],int64 A.[1])

let parseHeader (line:string)=
    line
    |> split
    |> Array.filter (fun s -> not(s=""))
  //  |> Array.map (fun s-> (int s))
    |> splitIntoValues

let parseLine (line:string) = 
    line
    |> split 
    |> splitIntoValues

let njobs= int x.[0]
let jobs = x |> Array.tail |> Array.map parseLine //  [value_1] [weight_1]

let mapjobs  (w,l) = (w-l,w,l)
let A = jobs |> Array.map mapjobs |> Array.sort |> Array.rev
let completion_time = A |> Array.scan (fun (x,acc) (a,b,c) -> (b,acc+c) ) (0L,0L)
let ans = completion_time |> Array.fold (fun acc (w,c) -> acc + w*c) 0L
printfn "answer  = %A" ans

let mapjobs1  (w,l) = (float w/float l,w,l)
let A1 = jobs |> Array.map mapjobs1 |> Array.sort |> Array.rev
let completion_time1 = A1 |> Array.scan (fun (x,acc) (a,b,c) -> (b,acc+c) ) (0L,0L)
let ans1 = completion_time1 |> Array.fold (fun acc (w,c) -> acc + w*c) 0L
printfn "answer1 = %A" ans1

// answer  = 69119377652L
// answer1 = 67311454237L