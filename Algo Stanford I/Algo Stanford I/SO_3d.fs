//////////////////// CPS style //////////////////////////

///////////////// preparing the data ////////////////////

open System
open System.Collections.Generic
open System.IO

let y =
   [|(1, 4); (2, 8); (3, 6); (4, 7); (5, 2); (6, 9); (7, 1); (8, 5); (8, 6);
    (9, 7); (9, 3)|]

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

for i in directgraphcore.Keys do
    if reversegraphcore.ContainsKey(i) then do

               let node = {children = reversegraphcore.[i] ;
                           finishingtime = -1 ;
                           explored1 = false ;
                           }
               reversegraph1.Add (i,node)
    
        else                                   
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

     for i in num_nodes .. -1 .. 1 do
        // printfn "%d" i
        if not(G.[i].explored1) then do 
                                    s <- i
                                    DFSsub G i id                                                         
                                                    
        printfn "%d %d" i G.[i].finishingtime

 
DFSLoop1 reversegraph1
printfn "faré"
printfn "pause"
Console.ReadKey() |> ignore

