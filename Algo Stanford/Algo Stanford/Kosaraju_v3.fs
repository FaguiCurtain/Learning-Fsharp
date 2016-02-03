﻿// it seems to be working on the small examples
// but its causing overflow errors due to non-tail recursivity

open System
open System.Collections.Generic
open System.IO

// let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford I\PA 4 - SCC.txt";;
let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford I\PA 4 - test1.txt";;
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


let DFSLoop1 (G:DFSgraph1)  = 
     let mutable t = 0
     let mutable s = -1

     let rec iter (n:int) (f:'a->unit) (list:'a list) :unit = 
         match list with 
            | [] -> (t <- t+1) ; (G.[n].finishingtime <- t)
            | x::xs -> (f x); (iter n f xs)
     let rec DFSsub (G:DFSgraph1) (n:int)=  
          let my_f (j:int):unit = if not(G.[j].explored1) then (DFSsub G j) else ()
          G.[n].explored1 <- true         
          iter n my_f (G.[n].children |> Array.toList)

           
     // end of DFSsub

     for i in num_nodes .. -1 .. 1 do
        printfn "%d" i
        if not(G.[i].explored1) then do 
                                    s <- i
                                    DFSsub G i |> ignore
     //   printfn "%d %d" i G.[i].finishingtime

DFSLoop1 reversegraph1

printfn "pause"
Console.ReadKey() |> ignore

for i in directgraphcore.Keys do
    let node = {children = 
                       directgraphcore.[i]
                       |> Array.map (fun k -> reversegraph1.[k].finishingtime)  ;
                leader = -1 ;
                explored2= false ;
                }
    directgraph2.Add (reversegraph1.[i].finishingtime,node)

let z = 0

let DFSLoop2 (G:DFSgraph2)  = 
     let mutable t = 0
     let mutable s = -1
     let mutable k = num_nodes

     let rec DFSsub (G:DFSgraph2)(n:int) (cont:int->int) =
            
          G.[n].explored2 <- true
          G.[n].leader <- s
          for j in G.[n].children do
                       if not(G.[j].explored2) then DFSsub G j cont
          t<-t+1
          // G.[n].finishingtime <- t  
           
     // end of DFSsub

     for i in num_nodes .. -1 .. 1 do
        if not(G.[i].explored2) then do 
                                    s <- i
                                    ( DFSsub G i (fun s -> s) ) |> ignore
       // printfn "%d %d" i G.[i].leader

DFSLoop2 directgraph2

printfn "pause"
Console.ReadKey() |> ignore

// let okcode = DFSsub directgraph 9 9 (fun x -> x)

let table = [for i in directgraph2.Keys do yield directgraph2.[i].leader]
let results = table |> Seq.countBy id |> Seq.map snd |> Seq.toList |> List.sort |> List.rev
printfn "%A" results

printfn "pause"
Console.ReadKey() |> ignore

 

     