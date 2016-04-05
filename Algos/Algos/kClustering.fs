open System
open System.Collections.Generic
open System.IO

open FSharp.Core

open MSDN.FSharp // add above PriorityQueue.fs
open UnionFind // add above UnionFind.fs

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

///////////////// preparing the data /////////////////

let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\PA2 - clustering1.txt"
// answer = 106 !!! correct !!!!


// let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\PA2 - test2.txt"
// answer with 4 clusters = 1414

// file format 
// [number_of_nodes]
// [one_node_of_edge_1] [other_node_of_edge_1] [edge_1_cost]
// [one_node_of_edge_2] [other_node_of_edge_2] [edge_2_cost]...

// the cost for edges.txt is between -9998 and 9993

let split (text:string)=
    text.Split [|'\t';' '|]

let splitIntoKeyValue1 (A: 'T[]) =  
    (int A.[0],int A.[1])

let splitIntoKeyValue2 (A: 'T[]) =  
    (int A.[0], int A.[1], int A.[2])

let parseHeader (line:string)=
    line
    |> split
    |> Array.filter (fun s -> not(s=""))
  //  |> Array.map (fun s-> (int s))
    |> splitIntoKeyValue1

let parseLine (line:string)=
    line
    |> split
    |> Array.filter (fun s -> not(s=""))
  //  |> Array.map (fun s-> (int s))
    |> splitIntoKeyValue2

//////////////////////////////

let n_nodes = int x.[0]

let edges = x |> Array.tail |> Array.map parseLine |> Array.toList |> List.sortBy (fun (a,b,c)->c)

//let nodelist = List.append (list |> List.map (fun (x,y,z) -> x)) 
//                           (list |> List.map (fun (x,y,z) -> y))
//               |> List.distinct
//
//let graph = // do not use graph.[0] //(int * int) list []
//    let g = Array.create (n_nodes+1) []
//    for (x,y,z) in list do
//        g.[x]<-(y,z)::g.[x]
//        g.[y]<-(x,z)::g.[y]
//    g

let num_clusters = 4 // number of clusters
let num_step = n_nodes-num_clusters

let p = Partition(n_nodes)

let mutable union_count = 0

let rec makecluster (l: (int*int*int) list) : (int*int*int) list= // gives as an output the list of edges that are have not been processed
    if (union_count = num_step) then l
       else
             let (i,j,c)= l.Head
             if (p.union_by_rank(i,j)=true) then union_count<-union_count+1
             makecluster l.Tail

let output_list = makecluster edges 

p.print()

printfn "%A" output_list

//among the remaining edges, find the first one (with minimum cost as the list is sorted) between two different groups

let rec spacing (l:(int*int*int) list) : int = 
    let (i,j,c) = l.Head
    let x = p.find_compress(i)
    let y = p.find_compress(j)
    if (x<>y) then c else spacing (l.Tail)

let res = spacing output_list
printfn "res = %A" res



stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds
Console.ReadKey() |> ignore


      