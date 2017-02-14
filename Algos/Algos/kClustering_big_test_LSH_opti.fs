open System
open System.Collections
open System.Collections.Generic
open System.IO

open FSharp.Core

open UnionFind // add above UnionFind.fs

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

///////////////// preparing the data /////////////////

let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\PA2 - clustering_big.txt"
// let n_nodes = 10000 // answer = 9116, 8000ms , 1000ms for hashing
let n_nodes = 10000

// let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\Algo II - PA2 Q2 - test1.txt"
// 200,000 nodes with 24-bit labels
 
// answer = 6118
// runs in 149 sec with LSH let's try to speed up that !!!
// edges_table [(0,1215);(1,28594);(2,328982)]

// let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\PA2 - test2.txt"
// answer with 4 clusters = 1414

// file format 
// [number_of_nodes][number_of_bits_of_a_node_label]
// [first bit of node1]...[last bit of node1]
// [first bit of node2]...[last bit of node2]

let split (text:string)=
    text.Split [|'\t';' '|]

let splitIntoKeyValue1 (A: 'T[]) =  
    (int A.[0],int A.[1])

let splitIntoKeyValue2 (A: 'T[]) =  
    (int A.[0], int A.[1], int A.[2])

let parseHeader (line:string)=
    line
    |> split
    |> Array.filter (fun s -> not(s=""))
  //  |> Array.map (fun s-> (int s))
    |> splitIntoKeyValue1

// converts each binary string to a integer 
let parseLine (line:string)= 
    int ("0b"+ line.Replace(" ",""))
    
    
    
//////////////////////////////

// let (n_nodes,n_bits) = parseHeader x.[0]
let n_bits = parseHeader x.[0] |> snd

x.[0] <- "0" // hack // we don't use .[0]
let edges =  x |> Array.map parseLine
edges.[0] <- -1 // we don't use .[0]

// https://msdn.microsoft.com/en-us/library/dd233239.aspx raise
exception InnerError of string

///////////////// implementing LSH to get a list of closest nodes /////////////////////

let makehash (n:int) =
    let byte1 = n >>> 18                  // edge.[0..5]  
    let byte2 = (n>>>12) - ((n>>>18)<<<6) // edge.[6..11]
    let byte3 = (n>>> 6) - ((n>>>12)<<<6) // edge.[12..17]
    let byte4 = n - (n>>>6)<<<6           // edge.[18..23]

    let hash12 = (byte1)<<<6 + byte2
    let hash13 = (byte1)<<<6 + byte3
    let hash14 = (byte1)<<<6 + byte4
    let hash23 = (byte2)<<<6 + byte3
    let hash24 = (byte2)<<<6 + byte4
    let hash34 = (byte3)<<<6 + byte4

    (hash12,hash13,hash14,hash23,hash24,hash34)

let hash_array =  edges |> Array.map makehash // array of size n_nodes +1 [0..n_nodes]

// let hashtable1 = new Hashtable (200000,float32 0.2);; pourquoi ça plante ????

let hashtable12 = new Dictionary<int,int list>() //don't forget the brackets
let hashtable13 = new Dictionary<int,int list>()
let hashtable14 = new Dictionary<int,int list>()
let hashtable23 = new Dictionary<int,int list>()
let hashtable24 = new Dictionary<int,int list>()
let hashtable34 = new Dictionary<int,int list>()

let AddtoDict (dict:Dictionary<int,int list>) (k,v) =
    if not(dict.ContainsKey k) then 
                              let value = [v]
                              dict.Add(k,value)
                            else
                               let v'= dict.[k]
                               dict.[k] <-  v::v' // dictionary is mutable

for i in 1..n_nodes do 
    AddtoDict hashtable12 ((hash_array.[i] |> (fun (a,b,c,d,e,f)->a)),i)
    AddtoDict hashtable13 ((hash_array.[i] |> (fun (a,b,c,d,e,f)->b)),i)
    AddtoDict hashtable14 ((hash_array.[i] |> (fun (a,b,c,d,e,f)->c)),i)
    AddtoDict hashtable23 ((hash_array.[i] |> (fun (a,b,c,d,e,f)->d)),i)
    AddtoDict hashtable24 ((hash_array.[i] |> (fun (a,b,c,d,e,f)->e)),i)
    AddtoDict hashtable34 ((hash_array.[i] |> (fun (a,b,c,d,e,f)->f)),i)

let keylist12 = hashtable12.Keys |> Seq.toList
let keylist13 = hashtable13.Keys |> Seq.toList
let keylist14 = hashtable14.Keys |> Seq.toList
let keylist23 = hashtable23.Keys |> Seq.toList
let keylist24 = hashtable24.Keys |> Seq.toList
let keylist34 = hashtable34.Keys |> Seq.toList

stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds

let stopWatch1 = System.Diagnostics.Stopwatch.StartNew()

// hashtable format =
// list of (hashkey, list of [nodes hashing to the same key])

//returns a table with the number of occurences of length sizes for the entries of the Dictionary
// example of recursing thru a list indexing a Dict to create a list
let keycount_table (hashtable:Dictionary<int,int list>) (keylist:int list) : (int*int) list=     
    let rec helper (l:int list) (listacc:int list) = 
       match l with
          | []   -> listacc
          | h::t -> let len = hashtable.[h] |> List.length
                    helper t (len::listacc)
    helper keylist [] |> Seq.countBy id |> Seq.toList
 
//  keycount_table hashtable12 keylist12;;
//val it : (int * int) list =
//  [(53, 201); (54, 148); (42, 163); (34, 19); (38, 75); (37, 56); (33, 18);
//   (48, 243); (43, 179); (40, 114); (47, 225); (41, 131); (36, 29); (46, 228);
//   (62, 44); (39, 83); (56, 127); (27, 4); (45, 196); (50, 245); (44, 198);
//   (52, 216); (51, 232); (35, 26); (60, 65); (49, 229); (57, 106); (58, 95);
//   (55, 135); (59, 77); (61, 53); (65, 10); (63, 28); (64, 29); (31, 6);
//   (32, 10); (68, 9); (71, 2); (69, 6); (30, 4); (66, 12); (28, 2); (77, 2);
//   (67, 8); (72, 4); (74, 1); (70, 2); (29, 1)]
// horrible result !!!! too many candidates !!!!

//let rec intToBinary i =
//    match i with
//    | 0 | 1 -> string i
//    | _ ->
//        let bit = string (i % 2)
//        (intToBinary (i / 2)) + bit 
//    
//let hammond_distance (u:int) (v:int):int =
//    (u^^^v) |> intToBinary |> 
//       Seq.fold (fun acc c ->
//                     match c with
//                       |'1' ->  acc+1
//                       |'0' ->  acc+0
//                       | _ -> acc // should not happen
//                 ) 0

let rec sumbits (n:int):int=
    let rec helper acc m =
       match m with
         | 0 -> acc
         | 1 -> acc+1 // enlever cela ?
         | _ -> let r = m%2
                helper (acc+r) (m>>>1)
    helper 0 n

let hammond_distance (u:int) (v:int):int =
    (u^^^v) |> sumbits


let get_hammond_distance i j =
    hammond_distance edges.[i] edges.[j]


let generate_distance_pair (nodelist:int list) =
     let rec main_helper (nl:int list) listacc =
         match nl with 
           | []   -> listacc // should not happen
           | [x]  -> listacc
           | h::t -> List.fold (fun l_acc node->  (h,node,get_hammond_distance h node)::l_acc) listacc t
                     |> main_helper t
     main_helper nodelist []             

// example of recursing thru a list indexing a Dict to create a list  //                    
let make_Hdistance_list (hashtable:Dictionary<int,int list>) (keylist:int list) : (int*int*int) list=     
    let rec helper (l:int list) (listacc: (int*int*int) list) = 
       match l with
          | []   -> listacc
          | h::t -> let elem = generate_distance_pair hashtable.[h]
                    helper t (List.append elem listacc)
    helper keylist []           
 
let Hdistance_list = 
                     let list12 = make_Hdistance_list (hashtable12) (keylist12)
                     let list13 = make_Hdistance_list (hashtable13) (keylist13)
                     let list14 = make_Hdistance_list (hashtable14) (keylist14)
                     let list23 = make_Hdistance_list (hashtable23) (keylist23)
                     let list24 = make_Hdistance_list (hashtable24) (keylist24)
                     let list34 = make_Hdistance_list (hashtable34) (keylist34)

                     List.concat [list12;list13;list14;list23;list24;list34]
                     |> List.sortBy (fun (a,b,c)->c)
// 916705 elements

let process_list = Hdistance_list |> List.filter (fun (a,b,c) -> c<=2) |> List.distinct
let process_edges_table = process_list |> List.map (fun (a,b,c)-> c) |> Seq.countBy id |> Seq.toList

printfn "edges_table %A "  process_edges_table    

stopWatch1.Stop()
printfn "%f" stopWatch1.Elapsed.TotalMilliseconds
//////////////////////////////////////////

let stopWatch2 = System.Diagnostics.Stopwatch.StartNew()

let p = Partition(n_nodes)

let mutable union_count = 0

let rec makecluster (l: (int*int*int) list) : (int*int*int) list= // gives as an output the list of edges that are have not been processed
    if l = [] then [] else
       let (i,j,c)= l.Head
       if (c>2) then l
                else
                    if (p.union_by_rank(i,j)=true) then union_count<-union_count+1
                    makecluster l.Tail

// here we use a non exhaustive list of distances

let output_list = makecluster process_list 

// p.print()

// printfn "%A" output_list

//among the remaining edges, find the first one (with minimum cost as the list is sorted) between two different groups

let rec spacing (l:(int*int*int) list) : int = 
    if l = [] then 0 else
       let (i,j,c) = l.Head
       let x = p.find_compress(i)
       let y = p.find_compress(j)
       if (x<>y) then c else spacing (l.Tail)

let res = spacing output_list
printfn "res = %A" res

let num_clusters = (p.output() |> Array.map snd |> Seq.distinct |> Seq.length) - 1 // need to remove 0
printfn "number of clusters = %A" num_clusters


stopWatch2.Stop()
printfn "%f" stopWatch2.Elapsed.TotalMilliseconds
Console.ReadKey() |> ignore


      