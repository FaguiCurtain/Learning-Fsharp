//////////////////// CPS style //////////////////////////
// make sure the compiler is well setup to make the functions tail-recursive
// else there will be a stack overflow problem
// if using VS2015, in the Solution Explorer, right click on the solution name,
// then on properties, click on Build, and tick up the box "Generate tail calls"

///////////////// preparing the data ////////////////////

open System
open System.Collections.Generic
open System.IO

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\PA 4 - SCC.txt";;
// let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\PA 4 - test1.txt";;
// val x : string [] =

let splitAtTab (text:string)=
    text.Split [|'\t';' '|]

let splitIntoKeyValue (A: int[]) = 
    (A.[0], A.[1])

let parseLine (line:string)=
    line
    |> splitAtTab
    |> Array.filter (fun s -> not(s=""))
    |> Array.map (fun s-> (int s))
    |> splitIntoKeyValue

let y =
  x |> Array.map parseLine

let maxnum_nodes =
       y |> Array.map (fun z -> max (fst z) (snd z)) |> Array.max

//let y =
//   [|(1, 4); (2, 8); (3, 6); (4, 7); (5, 2); (6, 9); (7, 1); (8, 5); (8, 6);
//    (9, 7); (9, 3)|]

// let y = Array.append [|(1,1);(1,2);(2,3);(3,1)|] [|for i in 4 .. 10000 do yield (i,4)|] 
// let y = Array.append [|(1,1);(1,2);(2,3);(3,1)|] [|for i in 4 .. 14999 do yield (i,i+1)|] 

 //val it : (int * int) [] 

type Children = int list
type Node1 =  
     {children : Children ;
      mutable finishingtime : int ;
      mutable explored1 : bool ; 
      }

type Node2 = 
     {children : Children ;
      mutable leader : int ;
      mutable explored2 : bool ; 
      }

// we will never use i = 0

type DFSgraphcore    = Children[]
let (directgraphcore:DFSgraphcore) = 
    [| for i in 0..maxnum_nodes do yield [] |]
let (reversegraphcore:DFSgraphcore) = 
    [| for i in 0..maxnum_nodes do yield [] |]

type DFSgraph1    = Node1[]
let (reversegraph1:DFSgraph1) = 
    [| for i in 0..maxnum_nodes do yield {children = [];
                                          finishingtime = -1;
                                          explored1=false} |]

type DFSgraph2    = Node2[]
let (directgraph2 :DFSgraph2) = 
    [| for i in 0..maxnum_nodes do yield {children = [];
                                          leader = -1;
                                          explored2 = false} |]

let AddtoGraph (G:DFSgraphcore) (n,c) = 
    match G.[n] with
      | [] -> (G.[n] <- [c])
      | c'-> (G.[n] <- c::c')
                               
let inline swaptuple (a,b) = (b,a)

y|> Array.iter (AddtoGraph directgraphcore)
y|> Array.map swaptuple |> Array.iter (AddtoGraph reversegraphcore)


// définir reversegraph1 = ... with....
for i in 1..maxnum_nodes do
    let node = {children = reversegraphcore.[i] ;
                           finishingtime = -1 ;
                           explored1 = false ;
                           }
    reversegraph1.[i] <- node

// for i in reversegraph1.Keys do printfn "%d %A" i reversegraph1.[i].children
// printfn "finished creating data. pause"
// Console.ReadKey() |> ignore

//////////////////// main code is below ///////////////////
let DFSLoop1 (G:DFSgraph1)  = 
     let mutable t =  0 
     let mutable s =  -1

     let rec iter_c (n:int) (f_c:'a->(unit->'r)->'r) (list:'a list) (cont: unit->'r) : 'r = 
         match list with 
            | [] -> (t <- t+1) ; (G.[n].finishingtime <- t) ; cont()
            | x::xs -> f_c x (fun ()-> iter_c n f_c xs cont)
     let rec DFSsub (G:DFSgraph1) (n:int) (cont: unit->'r) : 'r=  
          let my_f_c (j:int)(cont:unit->'r):'r = if not(G.[j].explored1) then (DFSsub G j cont) else cont()
          G.[n].explored1 <- true         
          iter_c n my_f_c G.[n].children cont

     
     for i in maxnum_nodes .. -1 .. 1 do
       // printfn "%d" i
        if not(G.[i].explored1) then do 
                                    s <- i
                                    DFSsub G i id                                                         
                                                    
       // printfn "%d %d" i G.[i].finishingtime

 
DFSLoop1 reversegraph1

for i in 1..maxnum_nodes do

    if (directgraphcore.[i] = []) then do
       let node = {children = [];             
                   leader = -1 ;
                   explored2= false ;
                   }
       directgraph2.[reversegraph1.[i].finishingtime] <- node

                                           else
       let node = {children = 
                       directgraphcore.[i]
                       |> List.map (fun k -> reversegraph1.[k].finishingtime)  ;
                   leader = -1 ;
                   explored2= false ;
                   }
       directgraph2.[reversegraph1.[i].finishingtime] <- node

let z = 0

let DFSLoop2 (G:DFSgraph2)  = 
     let mutable t =  0 
     let mutable s =  -1

     let rec iter_c (n:int) (f_c:'a->(unit->'r)->'r) (list:'a list) (cont: unit->'r) : 'r = 
         match list with 
            | [] -> (t <- t+1) ; cont()
            | x::xs -> f_c x (fun ()-> iter_c n f_c xs cont)
     let rec DFSsub (G:DFSgraph2) (n:int) (cont: unit->'r) : 'r=  
          let my_f_c (j:int)(cont:unit->'r):'r = if not(G.[j].explored2) then (DFSsub G j cont) else cont()
          G.[n].explored2 <- true
          G.[n].leader <- s         
          iter_c n my_f_c G.[n].children cont

     
     for i in maxnum_nodes .. -1 .. 1 do
       // printfn "%d" i
        if not(G.[i].explored2) then do 
                                    s <- i
                                    DFSsub G i id                                                         
                                                    
       // printfn "%d %d" i G.[i].leader

DFSLoop2 directgraph2

// let okcode = DFSsub directgraph 9 9 (fun x -> x)

let table = [for i in 1 .. maxnum_nodes do yield directgraph2.[i].leader]
let results = table |> Seq.countBy id |> Seq.map snd |> Seq.toList |> List.sort |> List.rev
printfn "%A" results

printfn "pause"
stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds
Console.ReadKey() |> ignore

// 434821,968,459,313,211