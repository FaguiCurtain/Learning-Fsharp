// it seems to be working on the small examples
// but its causing overflow errors due to non-tail recursivity

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

let y =
    x |> Array.map parseLine
 //val it : (int * int) [] 

type Children = int[]
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

type DFSgraph2    = Dictionary<int,Node2>
let directgraph2  = new DFSgraph2()

let AddtoGraph (G:DFSgraphcore) (n,c) = 
    if not(G.ContainsKey n) then 
                              let node = [|c|]
                              G.Add(n,node)
                            else
                               let c'= G.[n]
                               G.Remove(n) |> ignore
                               G.Add (n, Array.append c' [|c|])
                               
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
               let node = {children = [||] ;
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


let rec DFSsub (G:DFSgraph1) s t (n:int) cont =
     //how to make it tail recursive ???
          
          G.[n].explored1 <- true
          // G.[n].leader <- s
          G.[n].children |> Array.iter (fun j -> if not(G.[j].explored1) then DFSsub G s t j cont
                                                                         
                                        )
          cont n

     // end of DFSsub

let DFSLoop1 (G:DFSgraph1)  = 
     let mutable t =  0
     let mutable s =  -1
     // let mutable k = num_nodes

     for i in num_nodes .. -1 .. 1 do
        printfn "%d" i
        if not(G.[i].explored1) then do 
                                    s <- i
                                    DFSsub G s t i (fun k -> (t <- t+1) 
                                                           |> fun () -> (G.[k].finishingtime <- t)
                                                    ) 
       // printfn "%d %d" i G.[i].finishingtime

// End of DFSLoop1

DFSLoop1 reversegraph1

printfn "pause"
Console.ReadKey() |> ignore