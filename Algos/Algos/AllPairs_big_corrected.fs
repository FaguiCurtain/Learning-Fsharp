//////// all pairs shortest path problems ////////


open System
open System.Collections
open System.Collections.Generic
open System.IO

open FSharp.Core

open Spreads
open Spreads.Collections


let stopWatch = System.Diagnostics.Stopwatch.StartNew()

///////////////// preparing the data /////////////////

// format of the files
//[number_of_vertices][number_of_edges]
//[tail_1] [head_1] [length_1] // length can be negative for this problem

// let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algos\Algos\Stanford Algo II\Algo II - PA4 - g3.txt"
// g3.txt has 1000 vertex and 47979 edges

// answer g1 = cycle; g2 = cycle; g3 = -19
// when there is a cycle, program stops in 15sec
// for g3 finished in less than 1sec

let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algos\Algos\Stanford Algo II\Algo II - PA4 - large_graph.txt"
// the large graph has 20000 edges and 999387 vertices

// the large graph has 1239 negative edges and 2330 distinct vertices from those edges
// objects |> Array.filter (fun (a,b,c) -> c<0) |> Array.length // 1239
// let y = objects |> Array.filter (fun (a,b,c) -> c<0) |> Array.map (fun(a,b,c)->(a,b))
// let folder = fun list (a,b) -> a::(b::list)
// y |> Array.fold folder [] |> List.distinct |> List.length // 2330
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



// answer for g3.txt
//res = (-19, (399, 904))

//let path_res= [399; 175; 177; 187; 200; 320; 225; 230; 79; 307; 193; 58; 182; 223; 179; 194;
// 217; 214; 36; 258; 327; 351; 109; 189; 291; 279; 372; 278; 314; 401; 423; 515;
// 289; 566; 625; 438; 292; 454; 736; 771; 810; 895; 904]
//
//let double_check path_list (graph: (int*int) list []) = 
//    let find_edge (i,j) = (List.find (fun (n,c)-> (n=j)) graph.[i]) |> snd
//    let A = path_list |> List.toArray 
//    let len = Array.length A
//    [| for i in 1..(len - 1) do yield (A.[i-1],A.[i])|] |> Array.map find_edge |> Array.sum
//
//double_check path_res graph

///////////////////////

let Edge_Array_to_Graph (edge_array:(int*int*int)[]) (N:int) = // N=num_vertices
    let g = Array.create (N+1) []
    let l = Array.length edge_array
    let mutable u=0
    let mutable v=0
    let mutable cost=0
    for i in 0..((Array.length edge_array) - 1) do 
        u <- edge_array.[i] |> fun (a,b,c)->a
        v <- edge_array.[i] |> fun (a,b,c)->b
        cost <- edge_array.[i] |> fun (a,b,c)->c //why doesn't this work ????  (u,v,cost) <- objects.[i] 
        g.[u] <- (v,cost)::g.[u]
    g

let Edge_Array_to_ReverseGraph (edge_array:(int*int*int)[]) (N:int) = // N=num_vertices
    let g = Array.create (N+1) []
    let l = Array.length edge_array
    let mutable u=0
    let mutable v=0
    let mutable cost=0
    for i in 0..((Array.length edge_array) - 1) do 
        u <- edge_array.[i] |> fun (a,b,c)->a
        v <- edge_array.[i] |> fun (a,b,c)->b
        cost <- edge_array.[i] |> fun (a,b,c)->c //why doesn't this work ????  (u,v,cost) <- objects.[i] 
        g.[v] <- (u,cost)::g.[v]
    g



///// Bellman-Ford Algorithm //////
///// solves shortest path single Source problem, returns an array with shortest distances for each possible destination
let dist_array (A:int[])(B:int[]) = 
    let folder acc x y = acc + abs(x-y)
    Array.fold2 folder 0 A B 


let BellmanFord (edge_array:(int*int*int)[]) (S:int) N = // S=source N=num_vertices, M=num_edges 
   let reversegraph = Edge_Array_to_ReverseGraph edge_array N
   let mutable A = Array.create (N+1) limit // we don't use .[0]
   let A'= Array.create (N+1) 0 // A.[i,v] is the answer of the subproblem with #edges <=i and destination v
                                    // but we use space optimization with A.[v]=A.[i-1,v] and A'.[v]=A.[i,v]

   A.[S]<-0 
  
//   for i in 1..(N-1) do
//       for v in ([1..N] |>List.filter (fun e->not(e=S))) do
//           // A.[i,v]<- min A.[i-1,v] ([for (u,c_uv) in reversegraph.[v] do yield A.[i-1,u]+c_uv ] |> List.min)
//           A'.[v]<- min A.[v] ([for (u,c_uv) in reversegraph.[v] do yield A.[u]+c_uv ] |> List.min)
//       A <- Array.copy A'
//   
   let mutable more = true
   let mutable i = 1
   let mutable d = -1
   while (more=true) do
       for v in ([1..N] |>List.filter (fun e->not(e=S))) do
           // A.[i,v]<- min A.[i-1,v] ([for (u,c_uv) in reversegraph.[v] do yield A.[i-1,u]+c_uv ] |> List.min)
           A'.[v]<- min A.[v] ([for (u,c_uv) in reversegraph.[v] do yield A.[u]+c_uv ] |> List.min)
       // checking for early termination and detecting negative cycles
       d <- dist_array A A'
       if d = 0 then more <- false // early termination, relaxing the number of edges is not providing any progress. 
       i <-i+1      
       if i > N then more <- false
                     if d<>0 then failwith "there is a negative cycle"
       A <- Array.copy A'
   printfn "number of iterations = %A" i
       // printfn "A.[%A] = %A " i A
   
   // printfn "res BellmanFord= %A" A
   A

////// test /////
//let res = BellmanFord objects 399 num_vertices
//printfn "res = %A" res
//res.[904] = -19 gives the good answer
/////////////////

////// Solving the shortest path problem with Johnson's algorithm

//// add an extra edge to the graph
let johnson_array = Array.append objects [|for i in 1..num_vertices do yield (num_vertices+1,i,0)|]
let magic_numbers = BellmanFord johnson_array (num_vertices+1) (num_vertices+1) //magic_numbers.[num_vertices+1]=0
//the minimal magic number IS the shortest shortest path distance !!!
// runs in 0.9 seconds for g3
// res=6 32,773vseconds !! (9 hours !!)
let res = Array.min magic_numbers
printfn "res = %A" res
printfn "%f" stopWatch.Elapsed.TotalMilliseconds

Console.ReadKey() |> ignore

/// let objects1 = [|(1,2,-2);(2,3,-1);(3,1,4);(3,4,2);(3,5,-3);(6,4,1);(6,5,-4)|]
 