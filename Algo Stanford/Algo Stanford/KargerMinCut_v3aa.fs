// in v3a i'm getting rid of seq and Records

open MyModule.Dict
open MyModule.Misc

open System.IO
let x = File.ReadAllLines "C:\Users\Fagui\Documents\coursera\Algorithms\Stanford I\PA 3 - kargerMinCut.txt";;
 // let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford I\PA3 - simplegraph1.txt";;
// val x : string [] = 

let splitAtTab (text:string)=
    text.Split [|'\t';' '|]

let splitIntoKeyValue A = 
    (Array.head A, Array.tail A)

let parseLine (line:string)=
    line
    |> splitAtTab
    |> Array.filter (fun s -> not(s=""))
    |> Array.map (fun s-> (int s))
    |> splitIntoKeyValue

let y =
    x |> Array.map parseLine
let N = y.Length // number of nodes
let Nruns = 50

open System.Collections.Generic
// let graph = new Map <int, int array>

let graphM = (0,[||])::(y |> List.ofArray) //immutable 
             |> List.map snd
let G = Map.ofList [for i in 1..N do yield (i,graphM.[i])]



//using arrays to model instead of maps (in v2)
// we will not use .[0]. Arrays and lists starts at .[0] in F#


let remove_table =  [for i in 1..N+1 do yield false] //immutable list

let label_head_table = [for i in 1..N+1 do yield i-1] 

let index_table = [for i in 1..N do yield i]


let label =  [for i in 1..N+1 do yield [|i-1|]]

let mutable min_cut = 1000000

let IsNotRemoved x = // 
    match x with 
    | false -> true
    | true  -> false

let rand = System.Random()

[<EntryPoint>]
let main argv = 
    let mutable u = 0
    let mutable v = 0
    let mutable r = 0
    let mutable N_cut = 1000000
    let mutable cluster_A_min = [|0|]
    let mutable cluster_B_min = [|0|]
   
    let mutable LiveNodeList = [0]

    // when i = 2 problems with mutability // solved

    for i in 1 .. Nruns do
         
         let Graph = Array.ofList graphM
         let RemoveTable = Array.ofList remove_table
         let Label = Array.ofList label
         let LabelHead = Array.ofList label_head_table

         // let IsNotRemoved1 WG i = 
         let IsNotRemoved1 i = 
             RemoveTable.[i] |>IsNotRemoved

         let GetLiveNode ()= //horrible en F# !!!
             index_table |> List.filter (fun i -> (IsNotRemoved1 i))

         printfn "%d" i
         for k in 1..(N-2) do
             LiveNodeList <- GetLiveNode() //horrible en F# !!!
             r <- rand.Next(0,N-k)
             u <- LiveNodeList.[r] //selecting a live node
             let uuu  = Graph.[u] |> Array.map (fun s -> LabelHead.[s] )
                                  |> Array.filter (IsNotRemoved1)
                                  //|> Seq.distinct
             let n_edge =  uuu |> Array.length
             let x = rand.Next(1,n_edge)
             let mutable ok = false //maybe we can take this out
             while not(ok) do
                  // selecting the edge from node u
                  v <- LabelHead.[Array.get (uuu) (x-1)]
                  
                  let vvv = Graph.[v]  |> Array.map (fun s -> LabelHead.[s] )
                                       |> Array.filter (IsNotRemoved1)
                                          // |> Seq.distinct
                  let zzz = S_SubsetC (Seq.concat [uuu;vvv] |> Seq.distinct) [u;v]
                  Graph.[u] <- (zzz |> Seq.toArray)

                  let lab_u = Label.[u]
                  let lab_v = Label.[v]
                  Label.[u] <- (Array.concat [lab_u;lab_v] |> Array.distinct)

                  if (k<N-1) then 
                      RemoveTable.[v]<-true
                      //updating Label_head for all members of Label.[v]
                      LabelHead.[v]<- u
                      for j in Label.[v] do
                          LabelHead.[j]<- u

                  ok <- true
                //  printfn "u= %d v=%d" u v // for debugging
             // end of for k in 1..(N-2)
         // counting cuts
         // u,v contain the 2 indexes of groupings
         let cluster_A = Label.[u]
         let cluster_B = (S_SubsetC (seq[for i in 1..N do yield i]) cluster_A) |> Seq.toArray // defined as complementary of A
         
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
             
             cluster_A_min <- cluster_A
             cluster_B_min <- cluster_B
    // end of for i in 1..Nruns

    printfn "the minimum number of cut is %d" min_cut
    System.Console.ReadKey() |>ignore
    0 // return an integer exit code