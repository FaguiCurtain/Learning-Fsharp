//////// all pairs shortest path problems ////////

open System
open System.Collections
open System.Collections.Generic
open System.IO

open FSharp.Core


let stopWatch = System.Diagnostics.Stopwatch.StartNew()

///////////////// preparing the data /////////////////

// format of the files
//[number_of_vertices][number_of_edges]
//[tail_1] [head_1] [length_1] // length can be negative for this problem

let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\PA4 - g3.txt"
// answer g1 = cycle; g2 = cycle; g3 = -19
// when there is a cycle, program stops in 15sec
// for g3 finished in 105 sec


let split (text:string)=
    text.Split [|'\t';' '|]

let splitInto2Values (A: string []) =  
    (int A.[0],int A.[1])

let splitInto3Values (A: string []) =  
    (int A.[0],int A.[1],int A.[2])

let parseHeader (line:string)=
    line
    |> split
    |> Array.filter (fun s -> not(s=""))
  //  |> Array.map (fun s-> (int s))
    |> splitInto2Values

let parseLine (line:string) = 
    line
    |> split 
    |> splitInto3Values
 

let (num_vertices,num_edges) = x.[0] |> parseHeader

x.[0]<- "0 0 0" //hack   
let objects = x |> Array.map parseLine //  [value_1] [weight_1][length_1]

// Array2D.fold doesn't exist, this is a custom function found on Stack Overflow
let foldi (folder: int -> int -> 'S -> 'T -> 'S) (state: 'S) (array: 'T[,]) =
    let mutable state = state
    for x in 0 .. Array2D.length1 array - 1 do
        for y in 0 .. Array2D.length2 array - 1 do
            state <- folder x y state (array.[x, y])
    state

///////////////// Floyd Warshall algorithm /////////////////
let limit = 10000000
let mutable A = Array2D.create (num_vertices+1) (num_vertices+1) limit // we don't use .[0]
let A'= Array2D.create (num_vertices+1) (num_vertices+1) limit

for i in 1..num_vertices do A.[i,i]<-0
let mutable u=0
let mutable v=0
let mutable cost=0
for i in 1..num_edges do 
   u <- objects.[i] |> fun (a,b,c)->a
   v <- objects.[i] |> fun (a,b,c)->b
   cost <- objects.[i] |> fun (a,b,c)->c //why doesn't this work ????  (u,v,cost) <- objects.[i] 
   A.[u,v]<-cost

let mutable negcycle = false
for k in 1..num_vertices do
    if not(negcycle) then do 
       for i in 1..num_vertices do
           for j in 1..num_vertices do
               A'.[i,j] <- min (A.[i,j]) (A.[i,k] + A.[k,j])
       A <- Array2D.copy A'  
       negcycle <- ( [| for i in 1..num_vertices do yield A.[i,i] |] |> Array.min  ) <0


// check for negative cycle

let folder_min_no_diag i j acc x = 
    if (i=j) then acc
             else min acc x
if negcycle then printfn "there is a negative cycle"
            else let res = foldi (folder_min_no_diag) 0 A
                 printfn "res = %A" res


printfn "%f" stopWatch.Elapsed.TotalMilliseconds
Console.ReadKey() |> ignore