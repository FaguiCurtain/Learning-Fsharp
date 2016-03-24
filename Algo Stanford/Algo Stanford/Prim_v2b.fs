///// implementation of heap-based Prim's algorithm for finding       /////
///// Minimum Spanning Tree (MST) in a connected non-directed graph   /////
///// see https://class.coursera.org/algo2-004/lecture/35             /////

open System
open System.Collections.Generic
open System.IO

open FSharp.Core
open FSharpx.Collections

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

///////////////// preparing the data /////////////////

// let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\PA1 - edges_test1.txt"
// answer = 37


let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\PA1 - edges.txt"
// file format 
// [number_of_nodes] [number_of_edges]
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

let (n_nodes,n_edges) = parseHeader x.[0]

let list = x |> Array.tail |> Array.map parseLine |> Array.toList

let nodelist = List.append (list |> List.map (fun (x,y,z) -> x)) 
                           (list |> List.map (fun (x,y,z) -> y))
               |> List.distinct

let graph = // do not use graph.[0] //(int * int) list []
    let g = Array.create (n_nodes+1) []
    for (x,y,z) in list do
        g.[x]<-(y,z)::g.[x]
        g.[y]<-(x,z)::g.[y]
    g

let checksum = graph |> Array.map (List.map snd) |> Array.map (List.sum) |> Array.sum
// val it : int = 976234 (or 976234 / 2 = 488117 if we count edge cost only once)

// returns the minimum + index of the minimum
let mini (s : (int*int) list) = 
         match s with 
            | [] -> (-1,(-1,-1))
            | _  -> s |> Seq.mapi (fun i x -> (i, x)) |> Seq.minBy snd

let maxi (s : (int*int) list) = 
         match s with 
            | [] -> (-1,(-1,-1))
            | _  -> s |> Seq.mapi (fun i x -> (i, x)) |> Seq.maxBy snd


 ///////////////// initializing the heap /////////////////

let limit = 1000000 // max distance limit
// let mutable k = 0
let inX = Array.create (n_nodes+1) false // true if node in X = already processed
inX.[0]<-true

let mutable heap = Heap<int*int>(false,0,E) // Key = distance to X ; Value = Node 

let PP () =
    let readheap = Seq.toArray heap
    for i in 0..(heap.Length - 1) do (printfn "heap %i %A" i readheap.[i])

let MST_edges = Array.create (n_nodes) (0,0,0) // list of edges in MST
let C = Array.create (n_nodes+1) (0,0) // initialement, enregistre pour chaque point le sommet vers lequel le coût est minimal
let heap_dynamic_info = Array.create (n_nodes+1) (0,limit) // enregistre le coût minimum de X à v: (minedge,mincost)

/////////////////////////////////////////////////
let init_loop(firstnode:int) :unit=
    let flip (a:int,b:int) = (b,a)
    let Listflip list = List.map (fun (a,b)->(b,a)) list

    let closest_neighbour = 
        graph.[firstnode] |> Listflip |> mini |> snd |> flip
    let cost = snd closest_neighbour
    let W = fst closest_neighbour
    
    MST_edges.[0]<-(0,firstnode,0)
    MST_edges.[1]<-(firstnode,W,cost)

    // create the heap
    // for each v in V the key is the lowest cost of an edge uv with u in X
    // we need to update the cost of all vertices in V connected to W

    // update W neighbours 
    graph.[W] |> 
      List.iter  
        ( fun (node,cost) -> 
             heap <- heap.Insert (cost,node)
             heap_dynamic_info.[node] <- (W,cost)  )          
    inX.[W] <- true

    // update u neighbours
    graph.[firstnode] |> 
      List.iter ( 
        fun (node,cost) -> 
           if (inX.[node]=true) then () 
                                elif (cost>= (snd heap_dynamic_info.[node]) ) then ()                                                       
                                else   let i = PQ.IndexOf (snd heap_dynamic_info.[node]) node
                                       if (i= -1) then () else PQ.RemoveAt i
                                       PQ.Enqueue cost node
                                       heap_dynamic_info.[node] <- (firstnode,cost)                                                             
                 )
    
    PQ.RemoveAt (PQ.IndexOf cost firstnode) |> ignore
    heap_dynamic_info.[0]<- (0,0)
    inX.[firstnode] <- true // ne pas oublier !!!
    

let one_loop (k:int) : unit = 
    // take the first element from the queue
    let z = PQ.Dequeue()
    let W = z.Value // outer vertex with cheapest crossing-edge to X
    let cost = z.Key
    let u = heap_dynamic_info.[W] |> fst // vertex in X connected to W with cheapest crossing-edge
    MST_edges.[k]<-(u,W,cost)

    // maintain the heap
    // for each v in V the key is the lowest cost of an edge uv with u in X
    // we need to update the cost of all vertices in V connected to W
    let update_list = graph.[W]
    update_list |> 
      List.iter ( 
        fun (node,cost) -> 
           if (inX.[node]=true) then () 
                                elif (cost> (snd heap_dynamic_info.[node]) ) then ()                                                       
                                else   let i = PQ.IndexOf (snd heap_dynamic_info.[node]) node
                                       if (i= -1) then () else PQ.RemoveAt i
                                       PQ.Enqueue cost node
                                       heap_dynamic_info.[node] <- (W,cost)
                )
    inX.[W] <- true
/////////////////////////////////////////////
init_loop(1)
for k in 2..140 do one_loop k 
// for k in 2..(n_nodes - 1) do one_loop k 

let res = heap_dynamic_info |> Array.map (snd >> int64)  |> Array.sum                                          
printfn "%A" res
printfn "%A" MST_edges
PP()


let B' = MST_edges|> Array.map (fun (a,b,c)-> if (a<b) then (a,b) else (b,a)) |> Array.sort

stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds
Console.ReadKey() |> ignore

// 967ms pour edges_test1

///////////////// debogage ////////////////////

let soluce = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\solution Prim.txt"
             |> Array.map split 
             |> Array.map (fun [|a;b;c|] -> (int b,int a,int c))

let check = [for i in 0..499 do if i = 0 then yield 0 
                                         elif (MST_edges.[i]=soluce.[i-1]) then yield 0 
                                         else yield i]
           |> List.filter (fun s-> s>0)

// for k in 2..140 do one_loop k 
// après 140 itérations bizarrement, le heap ne fonctionne pas bien !!!!
// PQ 0 [-7230, 309]
// PQ 146 [-7277, 308]