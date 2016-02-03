open MyModule.Dict

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
type Node =  
     {children : Children ;
      mutable finishingtime : int ;
      mutable leader : int ;
      mutable explored1 : bool ;
      mutable explored2 : bool 
      }

type DFSgraphcore    = Dictionary<int,Children>
let directgraphcore  = new DFSgraphcore()
let reversegraphcore = new DFSgraphcore()

type DFSgraph    = Dictionary<int,Node>
let directgraph  = new DFSgraph()
let reversegraph = new DFSgraph()

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
    let node = {children = directgraphcore.[i] ;
                finishingtime = -1 ;
                leader = -1 ;
                explored1 = false ;
                explored2 = false ;}
    directgraph.Add (i,node)

for i in reversegraphcore.Keys do
    let node = {children = directgraphcore.[i] ;
                finishingtime = -1 ;
                leader = -1 ;
                explored1 = false ;
                explored2 = false ;}
    reversegraph.Add (i,node)

directgraphcore.Clear  |> ignore
reversegraphcore.Clear |> ignore

for i in directgraph.Keys do printfn "%A" directgraph.[i].children
Console.ReadKey() |> ignore

let num_nodes =
    directgraphcore |> Seq.length


let DFSLoop1 (G:DFSgraph)  = 
    let mutable t = 0
    let mutable s = -1
    let mutable k = num_nodes

    let rec DFSsub (G:DFSgraph)(n:int)(k:int) cont =
    
        match k with 
          | 0 -> cont 1 
          | _ -> // do stuff on G 
                 G.[n].explored1 <- true
                 G.[n].leader <- s
                 for j in G.[n].children do
                       if not(G[j].explored1) then DFSsub G j (k-1) cont
                 t<-t+1
                 G.[n].finishingtime <- t  
                            
    // end of DFSsub
    for i in num_nodes .. -1 .. 1 do
        if not(G.[i].explored1) then do 
                                    s <- i
                                    ( DFSsub G i i (fun s -> ()) ) |> ignore
    

// let okcode = DFSsub directgraph 9 9 (fun x -> x)
 
 DFSLoop1 reversegraph |> ignore                 