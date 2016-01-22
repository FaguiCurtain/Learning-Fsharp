﻿//////////////////// CPS style //////////////////////////

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

     let end_rec = (fun k -> (t <- t+1) 
                               |> (fun ()-> (G.[k].finishingtime <- t) )
                   )
     // i have to declare rec iter INSIDE DFSLoop1 in this version
    
     let rec iterc n fc list (cont:unit -> 'a) = 
         
         match list with 
            | [] -> cont()
            | x::xs -> (fc x cont |> fun ()->
                                 let newCont = cont
                                 iterc n fc xs newCont
                        )
      // il semble que cont ne serve à rien !!!

     let rec DFSsubc (G:DFSgraph1) (n:int) (cont: unit -> 'b) =
     //how to make it tail recursive ???
          let my_f_c j (cont:unit->'b) = 
                       if not(G.[j].explored1) then (DFSsubc G j cont)

          G.[n].explored1 <- true
          // G.[n].leader <- s
          
          iterc n my_f_c G.[n].children id
                         
            // est ce que déjà on est tail récursif ?                 
     // end of DFSsub
     
     // main loop

     for i in num_nodes .. -1 .. 1 do
        // printfn "%d" i
        if not(G.[i].explored1) then do 
                                    s <- i
                                    let FirstCont = end_rec
                                    DFSsubc G i FirstCont                                                   
                                                    
        printfn "%d %d" i G.[i].finishingtime
        
// End of DFSLoop1
 
DFSLoop1 reversegraph1

printfn "pause"
Console.ReadKey() |> ignore
