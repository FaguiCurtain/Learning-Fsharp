//////////////////// acc style //////////////////////////

///////////////// preparing the data ////////////////////

open System
open System.Collections.Generic
open System.IO

let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford I\PA 4 - SCC.txt";;
// let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford I\PA 4 - test1.txt";;
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

// let y =
//    x |> Array.map parseLine

//let y =
//   [|(1, 4); (2, 8); (3, 6); (4, 7); (5, 2); (6, 9); (7, 1); (8, 5); (8, 6);
//    (9, 7); (9, 3)|]

// let y = Array.append [|(1,1);(1,2);(2,3);(3,1)|] [|for i in 4 .. 10000 do yield (i,4)|] 
let y = Array.append [|(1,1);(1,2);(2,3);(3,1)|] [|for i in 4 .. 99999 do yield (i,i+1)|] 



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

type DFSgraphcore    = Dictionary<int,Children>
let directgraphcore  = new DFSgraphcore()
let reversegraphcore = new DFSgraphcore()

type DFSgraph1    = Dictionary<int,Node1>
let reversegraph1 = new DFSgraph1()

let AddtoGraph (G:DFSgraphcore) (n,c) = 
    if not(G.ContainsKey n) then 
                              let node = [c]
                              G.Add(n,node)
                            else
                               let c'= G.[n]
                               G.Remove(n) |> ignore
                               G.Add (n, List.append c' [c])
                               
let inline swaptuple (a,b) = (b,a)
y|> Array.iter (AddtoGraph directgraphcore)
y|> Array.map swaptuple |> Array.iter (AddtoGraph reversegraphcore)

// définir reversegraph1 = ... with....
for i in reversegraphcore.Keys do
    let node = {children = reversegraphcore.[i] ;
                           finishingtime = -1 ;
                           explored1 = false ;
                           }
    reversegraph1.Add (i,node)

for i in directgraphcore.Keys do
    if not(reversegraphcore.ContainsKey(i)) then do                                 
               let node = {children = [] ;
                           finishingtime = -1 ;
                           explored1 = false ;
                           }
               reversegraph1.Add (i,node)

directgraphcore.Clear  |> ignore
reversegraphcore.Clear |> ignore

// for i in reversegraph1.Keys do printfn "%d %A" i reversegraph1.[i].children
printfn "pause"
Console.ReadKey() |> ignore

let num_nodes =
    directgraphcore |> Seq.length

//////////////////// main code is below ///////////////////
type acc_t = (int*int list) list

let DFSLoop1 (G:DFSgraph1)  = 
     let mutable t =  0 
     let mutable s =  -1 
     let rec cont (acc:acc_t) : unit = 
              match acc with
                 |[] -> ()
                 |(n,l)::more -> iter_a n l more                          
         and iter_a (n:int) (list:int list) (acc:acc_t) : unit = 
             match list with 
               | [] -> (t <- t+1) ; (G.[n].finishingtime <- t) ; cont acc
               | x::xs ->  f_a x ((n,xs)::acc)
         and f_a (j:int) (acc:acc_t) :unit = if not(G.[j].explored1) then (DFSsub j acc) else cont acc                                              
         and DFSsub (n:int)(acc:acc_t) : unit= 
             G.[n].explored1 <- true         
             iter_a n G.[n].children acc
     for i in num_nodes .. -1 .. 1 do
        // printfn "%d" i
        if not(G.[i].explored1) then do 
                                    s <- i
                                    DFSsub i []                                                        
                                                    
        printfn "%d %d" i G.[i].finishingtime

 
DFSLoop1 reversegraph1
printfn "version e avec accumulateur"
printfn "pause"
Console.ReadKey() |> ignore

