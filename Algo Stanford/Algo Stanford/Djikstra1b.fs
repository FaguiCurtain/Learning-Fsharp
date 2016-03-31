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
       |> Array.map (List.map (fun (A:'T[]) -> (int A.[0],int A.[1]) )))
                  

let N2 = graphcore |> Array.map (List.map fst) |> Array.map List.max |> Array.max // node max

let N=N2

// non-optimized construction

let graph = 
    let g = Array.create (N+1) []
    for i in 0..((Array.length nodelist)-1) do
        g.[nodelist.[i]] <- graphcore.[i]
    g


let reversegraph = // (int*int) list []
    let (rg:(int*int) list [])= Array.create (N+1) []
    for i in 1..N do
        graph.[i] |> List.iter (fun (node,value) -> rg.[node] <- (i,value)::rg.[node] )
    rg
        


/////////////////// DJIKSTRA ///////////////////
let limit = 1000000 // max distance limit
let S = 1 // Source
let V = [0..N] |> List.filter (fun s -> not(s=S));;
let A = Array.create (N+1) limit // on ne se sert pas de A.[0]
A.[S] <- 0

let C = Array.create (N+1) -1 // stores the index of the element in X nearest to an element in V.
let D = Array.create (N+1) limit // stores the value of Dijkstra criterion

let inX = Array.create (N+1) false // remembers if the node is in X (= has been processed)
inX.[S]<-true

let PQ = new SortedDeque<int*int>() // Key = distance to X ; Value = Node 
let GetIndexOf (heap:SortedDeque<int*int>) elem = 
    try Some (heap.IndexOfElement elem) with | :? System.ArgumentOutOfRangeException -> None;;



let init_loop () : unit =
    for node in V do
        PQ.Add (limit,node)
    for (node,dist_to_S) in graph.[S] do
         PQ.RemoveAt (PQ.IndexOfElement (limit,node)) |> ignore
         PQ.Add (dist_to_S,node) |> ignore
         C.[node]<-S
         D.[node]<-dist_to_S
init_loop()

let PP () =
    for i in 0..(PQ.Count-1) do (printfn "PQ %i %A" i PQ.[i]);;

 // code to double check everything and DEBUG
let check() : unit=
    let V = [0..N] |> List.filter (fun s-> (inX.[s]=false))
    let mutable temp = limit
    for k in V do 
        temp <- limit
        let check_list = reversegraph.[k] |> List.filter (fun (n,d) -> inX.[n]=true)     
        for (i,d_i) in check_list do
            let y = (A.[i]+d_i)
            if (y  < temp) then temp <- y else ()

        match GetIndexOf PQ (temp,k) with 
          | None -> printfn "error at node %d with temp=%d" k temp
                    printfn "check_list = %A" check_list
                    failwith "stopping program"
          | Some i -> ()

    
let one_loop() : int =
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
    // DEBUG check
    // check()                          
    // returns W as a result of one_loop
    W

for k in 1..N do // one_loop()
                 printfn "big loop k=%d" k
                 printfn "k= %d W=%d" k (one_loop())
                                          
printfn "%A" A
// printfn "%i,%i,%i,%i,%i,%i,%i,%i,%i,%i" A.[7] A.[37] A.[59] A.[82] A.[99] A.[115] A.[133] A.[165] A.[188] A.[197]
PP()

// stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds
Console.ReadKey() |> ignore

// Giving the Right Answer now

// with Djikstra1 and Microsoft PriorityQueue
// bugged answer 2599,2610,2947,2052,2367,2399,2029,2442,2610,3068//
// the right answer for A.[188] is 2505...

// solution from Python
let AA =[|1000000;0; 2971; 2644; 3056; 2525; 2818; 2599; 1875; 745; 3205; 1551; 2906; 2394; 1803; 2942; 1837; 3111; 2284; 1044; 2351; 3630; 4028; 2650; 3653; 2249; 2150; 1222; 2090; 3540; 2303; 3455; 3004; 2551; 2656; 998; 2236; 2610; 3548; 1851; 4091; 2732; 2040; 3312; 2142; 3438; 2937; 2979; 2757; 2437; 3152; 2503; 2817; 2420; 3369; 2862; 2609; 2857; 3668; 2947; 2592; 1676; 2573; 2498; 2047; 826; 3393; 2535; 4636; 3650; 743; 1265; 1539; 3007; 4286; 2720; 3220; 2298; 2795; 2806; 982; 2976; 2052; 3997; 2656; 1193; 2461; 1608; 3046; 3261; 2018; 2786; 647; 3542; 3415; 2186; 2398; 4248; 3515; 2367; 2970; 3536; 2478; 1826; 2551; 3368; 2303; 2540; 1169; 3140; 2317; 2535; 1759; 1899; 508; 2399; 3513; 2597; 2176; 1090; 2328; 2818; 1306; 2805; 2057; 2618; 1694; 3285; 1203; 676; 1820; 1445; 2468; 2029; 1257; 1533; 2417; 3599; 2494; 4101; 546; 1889; 2616; 2141; 2359; 648; 2682; 3464; 2873; 3109; 2183; 4159; 1832; 2080; 1831; 2001; 3013; 2143; 1376; 1627; 2403; 4772; 2556; 2124; 1693; 2442; 3814; 2630; 2038; 2776; 1365; 3929; 1990; 2069; 3558; 1432; 2279; 3829; 2435; 3691; 3027; 2345; 3807; 2145; 2703; 2884; 3806; 1151; 2505; 2340; 2596; 4123; 1737; 3136; 1073; 1707; 2417; 3068; 1724; 815; 2060|]

let B = [|for i in 0..200 do yield A.[i]-AA.[i]|];;
let BB = [0..N] |> List.filter (fun s-> not(B.[s]=0))
//val BB : int list = [10; 26; 95; 96; 101; 147; 157; 184; 188; 196]// index of nodes with differences
// Array.partition (fun s->not(s=0)) B
// [for i in BB do yield B.[i]] // donne un résultat similaire...
// 

 