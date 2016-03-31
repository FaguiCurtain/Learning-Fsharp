///// The Knapsack problem: pack the most valuable items with a volume constraint /////

open System
open System.Collections
open System.Collections.Generic
open System.IO

open FSharp.Core


let stopWatch = System.Diagnostics.Stopwatch.StartNew()

///////////////// preparing the data /////////////////
// format of the files
//[knapsack_size][number_of_items]
//[value_1] [weight_1]
//[value_2] [weight_2]

let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\PA3 - knapsack1.txt"

let split (text:string)=
    text.Split [|'\t';' '|]

let splitIntoValues (A: 'T[]) =  
    (int A.[0],int A.[1])

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
   
let objects = x |> Array.map parseLine //  [value_1] [weight_1]
let (knapsack_size,number_of_items) = objects.[0] // (10000,100)

let A = Array2D.create 101 10001 0

for i in 1..number_of_items do
    for x in 0..knapsack_size do 
        A.[i,x]<-max A.[i-1,x] ( A.[i-1,max 0 (x - (snd objects.[i])) ] + (fst objects.[i]) )

printfn "answer = " A.[100,10000]