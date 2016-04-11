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

let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algos\Algos\Stanford Algo II\Algo II - PA4 - g3.txt"
// answer g1 = cycle; g2 = cycle; g3 = -19
// when there is a cycle, program stops in 15sec
// for g3 finished in 105 sec
// let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\PA4 - large_graph.txt"
// the large graph has 1239 negative edges and 2330 distinct vertices from those edges
//   // 1239
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

let FloydWarshall (edge_array:(int*int*int)[]) N M = // N=num_vertices, M=num_edges 
    // A.[i,j,k] is the length of shortest path from i to j with all internal nodes in 1..k
    // however, for space optimization, we remember just A=A.[k-1] and A'=A.[k]

    let mutable A = Array2D.create (N+1) (N+1) limit // we don't use .[0]
    let A'= Array2D.create (N+1) (N+1) limit

    for i in 1..N do A.[i,i]<-0
    let mutable u=0
    let mutable v=0
    let mutable cost=0
    for i in 1..M do 
       u <- edge_array.[i] |> fun (a,b,c)->a
       v <- edge_array.[i] |> fun (a,b,c)->b
       cost <- edge_array.[i] |> fun (a,b,c)->c //why doesn't this work ????  (u,v,cost) <- objects.[i] 
       A.[u,v]<-cost

    let mutable negcycle = false
    let folder_min_no_diag i j acc x = 
        if (i=j) then acc
                 else min acc x

    for k in 1..N do
        if not(negcycle) then do 
           for i in 1..N do
               for j in 1..N do
                   A'.[i,j] <- min (A.[i,j]) (A.[i,k] + A.[k,j])
           A <- Array2D.copy A'  
           negcycle <- ( [| for i in 1..N do yield A.[i,i] |] |> Array.min  ) <0

// check for negative cycle
  
    if negcycle then printfn "there is a negative cycle"
                else let res = foldi (folder_min_no_diag) 0 A
                     printfn "res = %A" res

//////// end of FloydWarshall ///////


///// same algo as above but provides also the optimal path when there is no negcycle

let FloydWarshall_with_path (edge_array:(int*int*int)[]) N M = // N=num_vertices, M=num_edges 
    // A.[i,j,k] is the length of shortest path from i to j with all internal nodes in 1..k
    // however, for space optimization, we remember just A=A.[k-1] and A'=A.[k]

    let mutable A = Array2D.create (N+1) (N+1) limit // we don't use .[0]
    let A'= Array2D.create (N+1) (N+1) limit

    let B = Array2D.create (N+1) (N+1) -1 // B.[i,j] = max label of internal node on shortest path from i to j

    for i in 1..N do A.[i,i]<-0
    let mutable u=0
    let mutable v=0
    let mutable cost=0
    for i in 1..M do 
       u <- edge_array.[i] |> fun (a,b,c)->a
       v <- edge_array.[i] |> fun (a,b,c)->b
       cost <- edge_array.[i] |> fun (a,b,c)->c //why doesn't this work ????  (u,v,cost) <- objects.[i] 
       A.[u,v]<-cost

    let mutable negcycle = false
    let folder_min_no_diag_with_index i j (acc:int*(int*int)) (x:int) = 
        if (i=j) then acc
                 else if x< (fst acc) then (x,(i,j)) else acc

    for k in 1..N do
        if not(negcycle) then do 
           for i in 1..N do
               for j in 1..N do
                   // A'.[i,j] <- min (A.[i,j]) (A.[i,k] + A.[k,j])
                   let candi1 = A.[i,j]
                   let candi2 = A.[i,k] + A.[k,j]
                   if candi1 <=candi2 then A'.[i,j]<-candi1
                                      else A'.[i,j]<-candi2
                                           B.[i,j] <- k
           A <- Array2D.copy A'  
           negcycle <- ( [| for i in 1..N do yield A.[i,i] |] |> Array.min  ) <0

// check for negative cycle
  
    if negcycle then printfn "there is a negative cycle"
                else let res = foldi (folder_min_no_diag_with_index) (0,(0,0)) A
                     printfn "res = %A" res
                     //recompute the shortest path
                     let shortest_head = fst (snd res)
                     let shortest_tail = snd (snd res)
                     let rec recover_shortest_path i j : int list= //// be careful this is not tail-recursive
                         let k = B.[i,j]
                         if (k= -1) then [i;j]
                                 else List.append (recover_shortest_path i k) (List.tail (recover_shortest_path k j))
                     let path_res = recover_shortest_path shortest_head shortest_tail
                     printfn "path_res= %A" path_res
                         
//////// end of FloydWarshall_with_path //////////

// execute FloydWarshall

// FloydWarshall objects num_vertices num_edges
// FloydWarshall_with_path objects num_vertices num_edges

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

let BellmanFord (edge_array:(int*int*int)[]) (S:int) N = // S=source N=num_vertices, M=num_edges 
   let reversegraph = Edge_Array_to_ReverseGraph edge_array N
   let mutable A = Array.create (N+1) limit // we don't use .[0]
   let A'= Array.create (N+1) 0 // A.[i,v] is the answer of the subproblem with #edges <=i and destination v
                                    // but we use space optimization with A.[v]=A.[i-1,v] and A'.[v]=A.[i,v]

   A.[S]<-0 
   for i in 1..(N-1) do
       for v in ([1..N] |>List.filter (fun e->not(e=S))) do
           // A.[i,v]<- min A.[i-1,v] ([for (u,c_uv) in reversegraph.[v] do yield A.[i-1,u]+c_uv ] |> List.min)
           A'.[v]<- min A.[v] ([for (u,c_uv) in reversegraph.[v] do yield A.[u]+c_uv ] |> List.min)
       A <- Array.copy A'
       // printfn "A.[%A] = %A " i A
   
   // printfn "res BellmanFord= %A" A
   A

////// test /////
//let res = BellmanFord objects 399 num_vertices
//printfn "res = %A" res
//res.[904] = -19 gives the good answer
/////////////////


printfn "%f" stopWatch.Elapsed.TotalMilliseconds
Console.ReadKey() |> ignore

/// let objects1 = [|(1,2,-2);(2,3,-1);(3,1,4);(3,4,2);(3,5,-3);(6,4,1);(6,5,-4)|]

// the large graph has 1239 negative edges and 2330 distinct vertices from those edges
//   // 1239
let neg_edges = objects |> Array.filter (fun (a,b,c) -> c<0) |> Array.map (fun(a,b,c)->(a,b))
let folder_out = fun setacc (a,b)  -> (Set.add a setacc )
let folder_in  = fun setacc (a,b)  -> (Set.add b setacc )
let vertices_with_out_neg_edges = neg_edges |> Array.fold folder_out Set.empty<int>  // |> Set.count2330
let vertices_with_in_neg_edges = neg_edges |> Array.fold folder_in Set.empty<int> // |> Set.count // 2330
let start_candi = Set.filter (fun e -> (Set.contains e vertices_with_in_neg_edges)=false) vertices_with_out_neg_edges
let end_candi = Set.filter (fun e -> (Set.contains e vertices_with_out_neg_edges)=false) vertices_with_in_neg_edges


//let BellmanFord_adapted (edge_array:(int*int*int)[]) (S:int) N = // S=source N=num_vertices, M=num_edges 
//   let reversegraph = Edge_Array_to_ReverseGraph edge_array N
//   let mutable A = Array.create (N+1) limit // we don't use .[0]
//   let A'= Array.create (N+1) 0 // A.[i,v] is the answer of the subproblem with #edges <=i and destination v
//                                    // but we use space optimization with A.[v]=A.[i-1,v] and A'.[v]=A.[i,v]
//
//   A.[S]<-0 
//   let mutable i = 1
//   let mutable more = true
//   let mutable active_list = ([1..N] |>List.filter (fun e->not(e=S)))
//   while (more=true) do  
//       for v in active_list do
//           // A.[i,v]<- min A.[i-1,v] ([for (u,c_uv) in reversegraph.[v] do yield A.[i-1,u]+c_uv ] |> List.min)
//           A'.[v]<- min A.[v] ([for (u,c_uv) in reversegraph.[v] do yield A.[u]+c_uv ] |> List.min)
//       A <- Array.copy A'
//       i <- i+1
//       active_list <- active_list |> List.filter (fun e->)
//       more <- i<=(N-1) && Array.min (Array.tail A) <0
//       // printfn "A.[%A] = %A " i A
//   // printfn "res BellmanFord= %A" A
//   printfn "i=%A" i
//   A
//
//let mutable m = limit
//let mutable tmp =0
//for u in vertices_with_neg_edges do
//    tmp <- Array.min( Array.tail (BellmanFord_adapted objects u num_vertices))
//    if tmp <m then m<-tmp
//printfn "shortest path distance = %A" m