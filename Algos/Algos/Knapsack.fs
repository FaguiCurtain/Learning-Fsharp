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
let objects = [|(6,4);(3,4);(2,3);(4,2);(4,3)|]

let (knapsack_size,number_of_items) = objects.[0] // (10000,100)

// let A = Array2D.create 101 10001 0
let A = Array2D.create (number_of_items + 1) (knapsack_size + 1) 0

let mutable weight = 0
let mutable value = 0

for i in 1..number_of_items do
    for x in 0..knapsack_size do 
        weight <- snd objects.[i]
        value <- fst objects.[i]
        if x>= weight then 
               A.[i,x]<-max A.[i-1,x] ( A.[i-1, (x - weight) ] + value )
                      else
               A.[i,x]<- A.[i-1,x]

printfn "answer = %A " A.[number_of_items,knapsack_size]

// correct answer for Q1 with knapsack1.txt (10000,100) = 2493893