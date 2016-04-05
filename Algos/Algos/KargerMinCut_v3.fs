open MyModule.Dict
open MyModule.Misc

open System.IO
 //let x = File.ReadAllLines "C:\Users\Fagui\Documents\coursera\Algorithms\Stanford I\PA 3 - kargerMinCut.txt";;
 let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford I\PA3 - simplegraph1.txt";;
// val x : string [] = 

let splitAtTab (text:string)=
    text.Split [|'\t';' '|]

let splitIntoKeyValue (s:seq<'T>) = 
    (Seq.head s, Seq.tail s)

let parseLine (line:string)=
    line
    |> splitAtTab
    |> Array.filter (fun s -> not(s=""))
    |> Array.map (fun s-> (int s))
    |> Array.toSeq
    |> splitIntoKeyValue

let y =
    x |> Array.map parseLine
let N = y.Length // number of nodes
let Nruns = 50

open System.Collections.Generic
// let graph = new Map <int, int array>

let graphM = (0,seq[])::(y |> List.ofArray) //immutable 
             |> List.map snd
let G = Map.ofList [for i in 1..N do yield (i,graphM.[i])]



//using arrays to model instead of maps (in v2)
// we will not use .[0]. Arrays and lists starts at .[0] in F#


let remove_table =  [for i in 1..N+1 do yield false] //immutable list

let label_head_table = [for i in 1..N+1 do yield i-1] 

let index_table = [for i in 1..N do yield i]


let label =  [for i in 1..N+1 do yield seq [i-1]]

let mutable min_cut = 1000000

type wgraphobj =
     { Graph : int seq[]
       RemoveTable : bool[]
       Label : int seq[]
       LabelHead : int[] }

let WG1 = {Graph = Array.ofList graphM
           RemoveTable = Array.ofList remove_table;
           Label = Array.ofList label;
           LabelHead = Array.ofList label_head_table}

let mutable WGmin = WG1

let IsNotRemoved x = // 
    match x with 
    | false -> true
    | true  -> false

let IsNotRemoved1 WG i = //
    (WG.RemoveTable.[i]) |>IsNotRemoved

let GetLiveNode WG = 
    
    index_table |> List.filter (fun i -> (IsNotRemoved1 WG i))

let rand = System.Random()

[<EntryPoint>]
let main argv = 
    let mutable u = 0
    let mutable v = 0
    let mutable r = 0
    let mutable N_cut = 1000000
    let mutable cluster_A_min = seq [0]
    let mutable cluster_B_min = seq [0]
    let mutable WG = WG1
    let mutable LiveNodeList = [0]

    // when i = 2, i encounter problems with mutability

    for i in 1 .. Nruns do
         // WG <- WG1
         WG <- {Graph = Array.ofList graphM;
                RemoveTable = Array.ofList remove_table;
                Label = Array.ofList label;
                LabelHead = Array.ofList label_head_table}

         printfn "%d" i
         for k in 1..(N-2) do
             LiveNodeList <- GetLiveNode WG
             r <- rand.Next(0,N-k)
             u <- LiveNodeList.[r] //selecting a live node
             let uuu  = WG.Graph.[u] |> Seq.map (fun s -> WG.LabelHead.[s] )
                                       |> Seq.filter (IsNotRemoved1 WG)
                                       //|> Seq.distinct
             let n_edge =  uuu |> Seq.length
             let x = rand.Next(1,n_edge)
             let mutable ok = false //maybe we can take this out
             while not(ok) do
                  // selecting the edge from node u
                  v <- WG.LabelHead.[Array.get (uuu |> Seq.toArray) (x-1)]
                  
                  let vvv = WG.Graph.[v]  |> Seq.map (fun s -> WG.LabelHead.[s] )
                                          |> Seq.filter (IsNotRemoved1 WG)
                                          // |> Seq.distinct
                  let zzz = S_SubsetC (Seq.concat [uuu;vvv] |> Seq.distinct) [u;v]
                  WG.Graph.[u] <- zzz

                  let lab_u = WG.Label.[u]
                  let lab_v = WG.Label.[v]
                  WG.Label.[u] <- Seq.concat [lab_u;lab_v] |> Seq.distinct

                  if (k<N-1) then 
                      WG.RemoveTable.[v]<-true
                      //updating Label_head for all members of Label.[v]
                      WG.LabelHead.[v]<- u
                      for j in WG.Label.[v] do
                          WG.LabelHead.[j]<- u

                  ok <- true
                //  printfn "u= %d v=%d" u v // for debugging
             // end of for k in 1..(N-2)
         // counting cuts
         // u,v contain the 2 indexes of groupings
         let cluster_A = WG.Label.[u]
         let cluster_B = S_SubsetC (seq[for i in 1..N do yield i]) cluster_A // defined as complementary of A
         // let WG2 = {Graph = D_Subset WG1.Graph (cluster_A |> Seq.toList)
         //          RemoveTable = remove_table
         //           Label = D_Subset WG1.Graph (cluster_A |> Seq.toList)
         //          LabelHead = label_head_table}
         let cross_edge = // returns keyvalue pair (k,S')
             let IsInCluster2 cluster (k,S) =
                 (k,S_Subset S cluster)                            
             (M_Subset G (cluster_A |> Seq.toList))
                    |> toSeq
                    |> Seq.map (IsInCluster2 cluster_B)
             
         N_cut <-
             cross_edge |> Seq.map (fun (k:int,v:int seq)-> Seq.length v)
                        |> Seq.sum
         printfn "N_cut = %d" N_cut

         if (N_cut<min_cut) then
             min_cut <- N_cut
             WGmin <- WG
             cluster_A_min <- cluster_A
             cluster_B_min <- cluster_B
    // end of for i in 1..Nruns

    printfn "the minimum number of cut is %d" min_cut
    System.Console.ReadKey() |>ignore
    0 // return an integer exit code