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


let MST_edges = Array.create (n_nodes) (0,0,0) // liste des edges inclus dans l'arbre optimal
let C = Array.create (n_nodes+1) (0,0) // initialement, enregistre pour chaque point le sommet vers lequel le coût est minimal
let heap_dynamic_info = Array.create (n_nodes+1) (0,limit) // enregistre le coût minimum de X à v: (minedge,mincost)



let init_loop() : int = // returns W the initial node
    let flip (a:int,b:int) = (b,a)
    let Listflip list = List.map (fun (a,b)->(b,a)) list

    for i in 1..n_nodes do // no real need for a heap, just the minimum is needed.
        let closest_neighbour =
            graph.[i] |> Listflip |> mini |> snd |> flip
        PQ0.Enqueue (snd closest_neighbour) i
        C.[i] <- closest_neighbour
    let z = PQ0.Dequeue() 
    let cost = z.Key
    let W = z.Value
    
    let u = C.[W] |> fst
    MST_edges.[0]<-(0,u,0)
    MST_edges.[1]<-(u,W,cost)

    // create the heap
    // for each v in V the key is the lowest cost of an edge uv with u in X
    // we need to update the cost of all vertices in V connected to W

    // update W neighbours 
    graph.[W] |> 
      List.iter  
        ( fun (node,cost) -> 
             PQ.Enqueue cost node
             heap_dynamic_info.[node] <- (W,cost)  )          
    inX.[W] <- true

    // update u neighbours
    graph.[u] |> 
      List.iter ( 
        fun (node,cost) -> 
           if (inX.[node]=true) then () 
                                elif (cost>= (snd heap_dynamic_info.[node]) ) then ()                                                       
                                else   let i = PQ.IndexOf (snd heap_dynamic_info.[node]) node
                                       if (i= -1) then () else PQ.RemoveAt i
                                       PQ.Enqueue cost node
                                       heap_dynamic_info.[node] <- (u,cost)                                                             
                 )
    
    PQ.RemoveAt (PQ.IndexOf cost u) |> ignore
    heap_dynamic_info.[0]<- (0,0)
    inX.[u] <- true // ne pas oublier !!!
    W

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
////////////////////////////////////
let W2 = init_loop() //304
for k in 2..(n_nodes - 1) do one_loop k

heap_dynamic_info.[W2]<-(0,0)

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
let sf = soluce |> Array.map split |> Array.map (fun [|a;b;c|] -> (int a,int b)) 
                 |> Array.map (fun (a,b) -> if (a<b) then (a,b) else (b,a))
                 |> Array.sort 
                 |> Array.toList
let B'' = Array.tail B' |> Array.toList
let check1 = B'' |> List.map (fun x -> List.tryFind (fun y-> y=x) sf) |> List.toArray
[|for i in 0..498 do yield (match check1.[i] with 
                                | Some (a,b) ->0
                                | None -> i ) |] |> Array.filter (fun s-> s>0)
// val it : int [] = [|362; 404|]
//> B''.[362];;
//val it : int * int = (283, 294)
//> B''.[404];;
//val it : int * int = (323, 324)
//> sf |> List.filter (fun (a,b) -> (a=323) || (b=323));;
//val it : (int * int) list = [(323, 398)]
//> B'' |> List.filter (fun (a,b) -> (a=323) || (b=323));;
//val it : (int * int) list = [(323, 324); (323, 398)]
//> sf |> List.filter (fun (a,b) -> (a=283) || (b=283));;
//val it : (int * int) list = [(282, 283); (283, 398); (283, 469)]
//> B'' |> List.filter (fun (a,b) -> (a=283) || (b=283));;
//val it : (int * int) list = [(282, 283); (283, 294); (283, 398); (283, 469)]

let check2 = sf |> List.map (fun x -> List.tryFind (fun y-> y=x) B'')

[|for i in 0..498 do yield (match check2.[i] with 
                                | Some (a,b) ->0
                                | None -> i ) |] |> Array.filter (fun s-> s>0)
//val it : int [] = [|35; 434|]
//> sf.[35];;
//val it : int * int = (22, 63)
//> sf.[434];;
//val it : int * int = (366, 382)

// MST_edges |> Array.tryFindIndex (fun (a,b,c)-> (a =366))
// MST_edges |> Array.tryFindIndex (fun (a,b,c)-> (b =382))