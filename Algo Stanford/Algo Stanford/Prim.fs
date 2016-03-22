open System
open System.Collections.Generic
open System.IO

open FSharp.Core

open MSDN.FSharp // add above PriorityQueue.fs


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
let mutable k = 0
let inX = Array.create (n_nodes+1) false // enregistre si le node est dans X, cad a été déjà traité
inX.[0]<-true

// PQ0 just for finding the cheapest edge in the graph

let PQ0 = new PriorityQueue<int,int>() // Key = distance to X ; Value = Node 

let PP0 () =
    for i in 0..(PQ0.Count-1) do (printfn "PQ0 %i %A" i PQ0.[i])


// PQ is the main heap

let PQ = new PriorityQueue<int,int>() // Key = distance to X ; Value = Node 

let PP () =
    for i in 0..(PQ.Count-1) do (printfn "PQ %i %A" i PQ.[i])

// for (x,y,z) in list do PQ.Enqueue z (x,y)


let B = Array.create (n_nodes) (0,0) // liste des edges inclus dans l'arbre optimal
let C = Array.create (n_nodes+1) (0,0) // initialement, enregistre pour chaque point le sommet vers lequel le coût est minimal
let D = Array.create (n_nodes+1) (0,limit) // enregistre le coût minimum de X à v: (minedge,mincost)

let mutable tmp = 0
// let mutable tmp1 = (0,0)
for i in 1..n_nodes do
    let tmp1 = (graph.[i] |> mini |> snd)
    PQ0.Enqueue (snd tmp1) i
    C.[i] <- tmp1



let init_loop() : int = // returns W the initial node
    let z = PQ0.Dequeue()
    let W = z.Value
    let cost = z.Key
    let u = C.[W] |> fst
    B.[1]<-(u,W)

    // create the heap
    // for each v in V the key is the lowest cost of an edge uv with u in X
    // we need to update the cost of all vertices in V connected to W

    let update_list1 = graph.[W]
    update_list1 |> List.iter ( fun (node,cost) -> PQ.Enqueue cost node
                                                   D.[node] <- (W,cost)
                              )
    inX.[W] <- true

    let update_list2 = graph.[u]
    update_list2 |> List.iter ( fun (node,cost) -> if (inX.[node]=true) then () 
                                                      elif (cost>= (snd D.[node]) ) then ()                                                       
                                                      else   let i = PQ.IndexOf (snd D.[node]) node
                                                             if (i= -1) then () else PQ.RemoveAt i
                                                             PQ.Enqueue cost node
                                                             D.[node] <- (u,cost)
                                                             
                              )
    
    PQ.RemoveAt (PQ.IndexOf cost u) |> ignore
    D.[0]<- (0,0)
    inX.[u] <- true // ne pas oublier !!!
    W

let one_loop (k:int) : unit =
    // take the first element from the queue
    let z = PQ.Dequeue()
    let W = z.Value
    let cost = z.Key
    let u = D.[W] |> fst
    B.[k]<-(u,W)

    // maintain the heap
    // for each v in V the key is the lowest cost of an edge uv with u in X
    // we need to update the cost of all vertices in V connected to W
    let update_list = graph.[W]
    update_list |> List.iter ( fun (node,cost) -> if (inX.[node]=true) then () 
                                                      elif (cost>= (snd D.[node]) ) then ()                                                       
                                                      else   let i = PQ.IndexOf (snd D.[node]) node
                                                             if (i= -1) then () else PQ.RemoveAt i
                                                             PQ.Enqueue cost node
                                                             D.[node] <- (W,cost)
                              )
    inX.[W] <- true

let W0 = init_loop()
for k in 2..(n_nodes - 1) do one_loop k

D.[W0]<-(0,0)

let res = D |> Array.map (snd >> int64)  |> Array.sum                                          
printfn "%A" res

PP()

stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds
Console.ReadKey() |> ignore