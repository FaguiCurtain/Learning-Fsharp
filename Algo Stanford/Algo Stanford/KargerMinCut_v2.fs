open MyModule.Dict

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

open System.Collections.Generic
// let graph = new Map <int, int array>
let graphD = new Dictionary<int,int seq>()
y |> Array.iter graphD.Add
let graphM = y |> Map.ofArray //immutable 

let N = y.Length // number of nodes
let Nruns = 50

let remove_table = Map.ofList [for i in 1..N do yield (i,false)]

let label_head_table = Map.ofList [for i in 1..N do yield (i,i)] 

let label = Map.ofList [for i in 1..N do yield (i,seq [i])]

let mutable min_cut = 1000000

type wgraphobj =
     { Graph : Dictionary<int,int seq>
       RemoveTable : Dictionary<int,bool>
       Label : Dictionary<int,int seq>
       LabelHead : Dictionary<int,int> }

let WG1 = {Graph = ofMap graphM;
          RemoveTable = ofMap remove_table;
          Label = ofMap label;
          LabelHead = ofMap label_head_table}

let mutable WGmin = WG1

let IsNotRemoved x = // 
    match x with 
    | (i,false) -> true
    | (i,true)  -> false

let IsNotRemoved1 WG i = //
    (i,WG.RemoveTable.[i]) |>IsNotRemoved

let GetLiveNode d = 
    let myfun x =
        match x with
        | (i,b) -> i
    d |> toList |> List.filter IsNotRemoved |> List.map myfun

let rand = System.Random()
// subsets a dictionary given a sub_list of keys
let D_Subset (dict:Dictionary<'T,'U>) (sub_list:list<'T>) = 
    let z = Dictionary<'T,'U>() // create new empty dictionary
    sub_list |> List.filter (fun k -> dict.ContainsKey k)
             |> List.map (fun k -> (k, dict.[k]))
             |> List.iter (fun s -> z.Add s)
    z

let M_Subset (map:Map<'T,'U>) (sub_list:list<'T>) = 
     Map.ofList (sub_list |> List.filter (fun k -> map.ContainsKey k)
                              |> List.map (fun k -> (k, map.[k]))
                 )
    
// subsets a dictionary given a sub_list of keys to remove
let D_SubsetC (dict:Dictionary<'T,'U>) (sub_list:list<'T>) =
    let z = dict
    sub_list |> List.filter (fun k -> dict.ContainsKey k)
                          |> List.map (fun k -> (dict.Remove k)) |>ignore
    z

// subsets a sequence by values in a sequence
let S_Subset (S:seq<'T>)(sub_list:seq<'T>) =
    S |> Seq.filter (fun s-> Seq.exists (fun elem -> elem = s) sub_list)

let S_SubsetC (S:seq<'T>)(sub_list:seq<'T>) =
    S |> Seq.filter (fun s-> not(Seq.exists (fun elem -> elem = s) sub_list))

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
         WG <- {Graph = ofMap graphM;
          RemoveTable = ofMap remove_table;
          Label = ofMap label;
          LabelHead = ofMap label_head_table}

         printfn "%d" i
         for k in 1..(N-2) do
             LiveNodeList <- GetLiveNode WG.RemoveTable
             r <- rand.Next(0,N-k)
             u <- LiveNodeList.[r] //selecting a live node
             let uuu  = WG.Graph.[u] |> Seq.map (fun s -> WG.LabelHead.[s] )
                                     |> Seq.filter (IsNotRemoved1 WG)
                                     |> Seq.distinct
             let n_edge =  uuu |> Seq.length
             let x = rand.Next(1,n_edge)
             let mutable ok = false //maybe we can take this out
             while not(ok) do
                  // selecting the edge from node u
                  v <- WG.LabelHead.[Array.get (uuu |> Seq.toArray) (x-1)]
                  
                  let vvv = WG.Graph.[v]  |> Seq.map (fun s -> WG.LabelHead.[s] )
                                          |> Seq.filter (IsNotRemoved1 WG)
                                          |> Seq.distinct
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
             //    printfn "u= %d v=%d" u v // for debugging
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
             (M_Subset graphM (cluster_A |> Seq.toList))
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