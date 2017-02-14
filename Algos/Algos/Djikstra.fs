///////////////// preparing the data ////////////////////

open System
open System.Collections.Generic
open System.IO

open MSDN.FSharp

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

// let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\PA 5 - dijkstraData.txt";;
let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\PA5 - test1.txt";;
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

let N1 = Array.max nodelist // attention si il y a un node image plus grand encore !!!


let graphcore = // (int*int) list [] // il va manquer les nodes qui n'ont pas de chemins sortants
    (y |> Array.map snd
       |> Array.map (List.map split_comma)
       |> Array.map (List.map (fun (A:'T[]) -> (int A.[0],int A.[1]) )))
                  

let N2 = graphcore |> Array.map (List.map fst) |> Array.map List.max |> Array.max // node max

let N=N2

// construction non optimisée, perte de temps
// rajoute un élément pour 0 dont on ne se sert pas           
let graph = 
    let g = Array.create (N+1) []
    for i in 0..((Array.length nodelist)-1) do
        g.[nodelist.[i]] <- graphcore.[i]
    g


let reversegraph = // (int*int) list []
    let (rg:(int*int) list [])= Array.create (N+1) []
    rg.[0] <- [(0,0)] 

    for i in 1..N do
        graph.[i] |> List.iter (fun (node,value) -> rg.[node] <- (i,value)::rg.[node] )
    rg
        
/////////////////// test ///////////////////

//let PQ1 = new PriorityQueue<int,int>()
//
//PQ1.Enqueue 2 3
//PQ1.Enqueue 3 4
//PQ1.Enqueue 1 5
//PQ1.Enqueue 1 6
//printfn "PriorityQueue %A" PQ1
//
//Console.ReadKey() |> ignore

/////////////////// DJIKSTRA ///////////////////
let limit = 1000000 // max distance limit
let X = [1] // list of processed nodes
let V = [2..N]
let A = Array.create (N+1) limit // on ne se sert pas de A.[0]
A.[0] <- -1
A.[1] <- 0

let C = Array.create (N+1) -1 // enregistre l'index de l'élément de X le plus proche d'un élément de V.
let D = Array.create (N+1) limit // enregistre le critère de Djikstra

let inX = Array.create (N+1) false // enregistre si le node est dans X, cad a été déjà traité
inX.[0]<-true
inX.[1]<-true

let PQ = new PriorityQueue<int,int>() // Key = distance to X ; Value = Node 

let init_loop () : unit =
    for node in V do
        PQ.Enqueue limit node 
    for (node,dist_to_one) in graph.[1] do
         PQ.RemoveAt (PQ.IndexOf limit node) |> ignore
         PQ.Enqueue dist_to_one node |> ignore
         C.[node]<-1
         D.[node]<-dist_to_one
init_loop()

let PP () =
    for i in 0..(PQ.Count-1) do (printfn "PQ %i %A" i PQ.[i]);;
    
let one_loop() : unit =
    // take the first element from the queue
    let z = PQ.Dequeue()
    let W = z.Value
    let l = z.Key
    A.[W]<- l
    // maintain the heap
    // the Key must the Djikstra criterion
    let update_list = graph.[W]
    update_list |> List.iter ( fun (node,dist) -> if (inX.[node]=true) then ()
                                                     else let x = l+dist                                                    
                                                          if x > D.[node] then ()
                                                              else PQ.RemoveAt (PQ.IndexOf D.[node] node) |> ignore // updater le node
                                                                   PQ.Enqueue x node
                                                                   C.[node]<- W
                                                                   D.[node]<- x
                              ) 
    inX.[W] <- true


for k in 2..N do one_loop()
                                          
printfn "%A" A
// printfn "A %i %i %i %i %i %i %i %i %i %i " A.[7] A.[37] A.[59] A.[82] A.[99] A.[115] A.[133] A.[165] A.[188] A.[197]
PP()

stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds
Console.ReadKey() |> ignore

