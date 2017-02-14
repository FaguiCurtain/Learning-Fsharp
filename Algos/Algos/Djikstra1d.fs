/////// solving Djikstra's shortest path algorithm in a directed graph ///////
/////// final version ///////

///////////////// preparing the data ////////////////////

open System
open System.Collections.Generic
open System.IO

open Spreads
open Spreads.Collections

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

// let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algos\Algos\Stanford Algo I\Algo I - PA5 - test4.txt"
let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algos\Algos\Stanford Algo I\Algo I - PA5 - dijkstraData.txt"

// val x : string [] =

// original format of each row is "row_number (a1,b1) (a2,b2) ....(an,bn)"

let split (text:string)=
    text.Split [|'\t';' '|]

let split_comma (text:string)=
    text.Split [|','|]


let splitIntoKeyValue (A: 'T[]) =  
    (A.[0], Seq.toList (Seq.tail A))

let parseLine (line:string)=
    line
    |> split
    |> Array.filter (fun s -> not(s=""))
  //  |> Array.map (fun s-> (int s))
    |> splitIntoKeyValue

let y =
  x |> Array.map parseLine

let nodelist = y |> Array.map fst |> Array.map int

let N1 = Array.max nodelist // be careful if there is an image node with a bigger ID !!!


let graphcore = // (int*int) list [] // nodes without outgoing edges will be missing
    (y |> Array.map snd
       |> Array.map (List.map split_comma)
       |> Array.map (List.map (fun (A:string[]) -> (int A.[0],int A.[1]) )))
                  

let N2 = graphcore |> Array.map (List.map fst) |> Array.map List.max |> Array.max // node max

let N=N2

// non-optimized construction

let graph = 
    let g = Array.create (N+1) []
    for i in 0..((Array.length nodelist)-1) do
        g.[nodelist.[i]] <- graphcore.[i]
    g

let graph1 = [| []; [(2,0)];[(3,0)];[(1,1);(4,0);(5,0)];[];[];[(5,2);(4,2)]|]      
let linear_graph2 = [| []; [(2,1)] ; [(3,1)];[(4,1)];[] |]
let graph3 =[| []; [(2,1);(3,1)] ; [(4,1)];[(4,1)];[] |]

/////////////////// DJIKSTRA ///////////////////
let limit = 1000000 // max distance limit

// computes all shortest path distances from the source S and returns it in an array
let Djikstra (graph: (int*int) list []) (S:int) (N:int)= // S = source

    let V = [0..N] |> List.filter (fun s -> not(s=S))
    let A = Array.create (N+1) limit // on ne se sert pas de A.[0]
    A.[S] <- 0

    let C = Array.create (N+1) -1 // stores the index of the element in X nearest to an element in V.
    let D = Array.create (N+1) limit // stores the value of Dijkstra criterion

    let inX = Array.create (N+1) false // remembers if the node is in X (= has been processed)
    inX.[S]<-true

    let PQ = new SortedDeque<int*int>() // Key = distance to X ; Value = Node 
    let GetIndexOf (heap:SortedDeque<int*int>) elem = 
        try Some (heap.IndexOfElement elem) with | :? System.ArgumentOutOfRangeException -> None

    let init_loop () : unit =
        for node in V do
            PQ.Add (limit,node)
        for (node,dist_to_S) in graph.[S] do
             PQ.RemoveAt (PQ.IndexOfElement (limit,node)) |> ignore
             PQ.Add (dist_to_S,node) |> ignore
             C.[node]<-S
             D.[node]<-dist_to_S
    init_loop()
    
    let one_loop() =
        // take the first element from the queue
        let z = PQ.RemoveFirst()
        let W = snd z
        let l = fst z
        A.[W]<- l
        // maintain the heap
        // the Key must the Dijkstra criterion
        let update_list = graph.[W]
        update_list 
          |> List.iter
               ( fun (node,dist) -> 
                    if (inX.[node]=true) 
                       then ()
                       else let x = l+dist                                                    
                            if x > D.[node] then ()
                                            else 
                                                match GetIndexOf PQ (D.[node],node) with 
                                                  | None -> printfn "error at node %d with temp=%d" node D.[node]
                                                            printfn "update_list = %A" update_list
                                                            failwith "stopping program"
                                                  | Some i ->  PQ.RemoveAt i |> ignore // updater le node                
                                                               PQ.Add (x,node)
                                                               C.[node]<- W // prédécesseur
                                                               D.[node]<- x // update la distance à X                                  
                              ) 
        inX.[W] <- true
    

    for k in 1..N do one_loop()
                                          
    A // returns the array of all shortest paths with source A.[0]=limit doesn't mean anything.
///// end of Djikstra ////

///// same as above but also returns a path with minimum distance (there may be more than one) 
let Djikstra_with_path (graph: (int*int) list []) (S:int) (N:int)= // S = source
    let V = [0..N] |> List.filter (fun s -> not(s=S))
    let A = Array.create (N+1) limit // on ne se sert pas de A.[0]
    A.[S] <- 0

    //
    let B = Array.create (N+1) [] 
    //
    let C = Array.create (N+1) -1 // stores the index of the element in X nearest to an element in V.
    let D = Array.create (N+1) limit // stores the value of Dijkstra criterion

    let inX = Array.create (N+1) false // remembers if the node is in X (= has been processed)
    inX.[S]<-true

    let PQ = new SortedDeque<int*int>() // Key = distance to X ; Value = Node 
    let GetIndexOf (heap:SortedDeque<int*int>) elem = 
        try Some (heap.IndexOfElement elem) with | :? System.ArgumentOutOfRangeException -> None

    let init_loop () : unit =
        for node in V do
            PQ.Add (limit,node)      
        for (node,dist_to_S) in graph.[S] do
             PQ.RemoveAt (PQ.IndexOfElement (limit,node)) |> ignore
             PQ.Add (dist_to_S,node) |> ignore
             C.[node]<-S
             D.[node]<-dist_to_S
             B.[node]<-[(S,node)]
    init_loop()
    // printfn "init ok"
    let one_loop() =
        // take the first element from the queue
        let z = PQ.RemoveFirst()
        let W = snd z
        let l = fst z
        A.[W]<- l
        // maintain the heap
        // the Key must the Dijkstra criterion
        let update_list = graph.[W]
        update_list 
          |> List.iter
               ( fun (node,dist) -> 
                    if (inX.[node]=true) 
                       then ()
                       else let x = l+dist                                                    
                            if x > D.[node] then ()
                                            else 
                                                match GetIndexOf PQ (D.[node],node) with 
                                                  | None -> printfn "error at node %d with temp=%d" node D.[node]
                                                            printfn "update_list = %A" update_list
                                                            failwith "stopping program"
                                                  | Some i ->  PQ.RemoveAt i |> ignore // updater le node                
                                                               PQ.Add (x,node)
                                                               C.[node]<- W // prédécesseur
                                                               D.[node]<- x // update la distance à X      
                                                               B.[node]<- (W,node)::B.[W]                          
                              ) 
        inX.[W] <- true
    

    for k in 1..N do one_loop()
                                        
    (A,(B|> Array.map List.rev)) // returns the array of all shortest paths with source A.[0]=limit doesn't mean anything.


// stopWatch.Stop()
let (A,B) = Djikstra_with_path graph 1 200
printfn "A = %A" A
printfn "B = %A" B
printfn "%f" stopWatch.Elapsed.TotalMilliseconds
Console.ReadKey() |> ignore


 