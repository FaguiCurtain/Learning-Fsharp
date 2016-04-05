/////// solving Djikstra's shortest path algorithm in a directed graph ///////


///////////////// preparing the data ////////////////////

open System
open System.Collections.Generic
open System.IO

open Spreads
open Spreads.Collections

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\PA5 - dijkstraData.txt"
// let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\PA5 - test4.txt"
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

// stopWatch.Stop()
let A = Djikstra graph 1 200
printfn "A = %A" A
printfn "%f" stopWatch.Elapsed.TotalMilliseconds
Console.ReadKey() |> ignore


 