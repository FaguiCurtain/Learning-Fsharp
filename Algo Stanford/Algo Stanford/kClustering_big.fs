open System
open System.Collections
open System.Collections.Generic
open System.IO

open FSharp.Core

open MSDN.FSharp // add above PriorityQueue.fs
open UnionFind // add above UnionFind.fs

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

///////////////// preparing the data /////////////////

let y = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\PA2 - clustering_big.txt"
let n_nodes = 666
let x = y.[0..n_nodes]

// answer = ???
// 200,000 nodes with 24-bit labels

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

let parseLine (line:string)=
    line
    |> split
    |> Array.filter (fun s -> not(s=""))
    |> Array.map  (fun s-> (int s))
    |> Array.map  (fun e-> (e=1)) 
    
//////////////////////////////

// let (n_nodes,n_bits) = parseHeader x.[0]
let n_bits = parseHeader x.[0] |> snd

let edges = x |> Array.map parseLine
edges.[0] <- [|for i in 1..n_bits do yield false|] // we don't use .[0]

// https://msdn.microsoft.com/en-us/library/dd233239.aspx raise
exception InnerError of string

//let rec power2 k = //for bit calculations only 
//    match k with 
//      | 0 -> 1
//      | _ -> if ((k >= 8) || (k<0)) then raise (InnerError("power exponent not allowed"))
//                else 2 * power2 (k-1)

let powers_of_2 = [|1;2;4;8;16;32;64;128;256;512;1024;2048;4096;8192;16384;32768|]

let power2 k = 
     if ((k >= 16) || (k<0)) then raise (InnerError("power exponent not allowed"))
        else powers_of_2.[k]

//let convert_to_byte bitarray : byte = //converts a boolean vector of size 8 into a byte
//    Array.fold (fun (acc,k) (bit:bool) -> let x = System.Convert.ToInt32(bit)
//                                          (acc+ x*(power2 k),k+1))
//               (0,0) bitarray
//    |> fst |> byte

let convert_bitarray_to_int bitarray : int = //converts a boolean vector of size (16) into an integer from 0 to 65535
    Array.fold (fun (acc,k) (bit:bool) -> let x = System.Convert.ToInt32(bit)
                                          (acc+ x*(power2 k),k+1))
               (0,0) bitarray
    |> fst


///////////////// implementing LSH to get a list of closest nodes /////////////////////
//// we have to be lucky and adjust manually some parameters...

let size_band = 8 // = r
let num_bands = 3 // number of bands = b
// b * r = n_bits
// 2^16 = 65536 ça fait un load facteur de 3 à peu près...

// http://theburningmonk.com/2011/12/f-from-string-to-byte-array-and-back/
//let makehash (edge:bool[]) =
//    let byte1 = edge.[0..7]   |> convert_to_byte
//    let byte2 = edge.[8..15]  |> convert_to_byte
//    let byte3 = edge.[16..23] |> convert_to_byte
//    let hash12 = System.Convert.ToBase64String [|byte1;byte2|]
//    let hash23 = System.Convert.ToBase64String [|byte2;byte3|]
//    let hash31 = System.Convert.ToBase64String [|byte3;byte1|]
//    // nota bene System.Convert.ToBase64String n'est pas commutatif
//    
//    (hash12,hash23,hash31)

let makehash (edge:bool[]) =
    let byte1 = edge.[0..7]  
    let byte2 = edge.[8..15]  
    let byte3 = edge.[16..23]
    let byte12 = Array.append byte1 byte2
    let byte23 = Array.append byte2 byte3
    let byte31 = Array.append byte3 byte1
    let hash12 = convert_bitarray_to_int byte12
    let hash23 = convert_bitarray_to_int byte23
    let hash31 = convert_bitarray_to_int byte31
    (hash12,hash23,hash31)

let hash_array =  edges |> Array.map makehash // array of size n_nodes +1 

// let hashtable1 = new Hashtable (200000,float32 0.2);;

let hashtable12 = new Dictionary<int,int list>() //don't forget the brackets
let hashtable23 = new Dictionary<int,int list>()
let hashtable31 = new Dictionary<int,int list>()

//let AddtoHashtable (hashtable:Hashtable) (k,v) =
//    if not(hashtable.ContainsKey k) then 
//                              let value = [v]
//                              hashtable.Add(k,value)
//                            else
//                               let v'= hashtable.[k]
//                               hashtable.Remove(k) |> ignore
//                               hashtable.Add (k, v::v')

let AddtoDict (dict:Dictionary<int,int list>) (k,v) =
    if not(dict.ContainsKey k) then 
                              let value = [v]
                              dict.Add(k,value)
                            else
                               let v'= dict.[k]
                               dict.[k] <-  v::v' // dictionary is mutable

for i in 1..n_nodes do 
    AddtoDict hashtable12 ((hash_array.[i] |> (fun (a,b,c)->a)),i)
    AddtoDict hashtable23 ((hash_array.[i] |> (fun (a,b,c)->b)),i)
    AddtoDict hashtable31 ((hash_array.[i] |> (fun (a,b,c)->c)),i)

let keylist12 = hashtable12.Keys |> Seq.toList
let keylist23 = hashtable23.Keys |> Seq.toList
let keylist31 = hashtable31.Keys |> Seq.toList

//returns a table with the number of occurences of length sizes for the entries of the Dictionary
let keycount_table (hashtable:Dictionary<int,int list>) (keylist:int list) : (int*int) list=     
    let rec helper (l:int list) (listacc:int list) = 
       match l with
          | []   -> listacc
          | h::t -> let len = hashtable.[h] |> List.length
                    helper t (len::listacc)
    helper keylist [] |> Seq.countBy id |> Seq.toList
 
let hammond_distance (u:bool[]) (v:bool[]):int =
    let size_u = u.Length
    let size_v = v.Length
    if (size_u <> size_v) then raise (InnerError("boolean vectors of different sizes"))
                          else  Array.map2 (fun b1 b2 -> b1 && b2) u v 
                                |> Array.fold (fun acc x -> match x with 
                                                              | true  -> acc+1
                                                              | false -> acc) 0
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
                       
let make_Hdistance_list (hashtable:Dictionary<int,int list>) (keylist:int list) : (int*int*int) list=     
    let rec helper (l:int list) (listacc: (int*int*int) list) = 
       match l with
          | []   -> listacc
          | h::t -> let elem = generate_distance_pair hashtable.[h]
                    helper t (List.append elem listacc)
    helper keylist []           
 
let Hdistance_list = 
                     let list12 = make_Hdistance_list (hashtable12) (keylist12)
                     let list23 = make_Hdistance_list (hashtable23) (keylist23)
                     let list31 = make_Hdistance_list (hashtable31) (keylist31)
                     List.append list12 (List.append list23 list31)
                     |> List.sortBy (fun (a,b,c)->c)
// 916705 elements
                                
//////////////////////////////////////////

let p = Partition(n_nodes)

let mutable union_count = 0

let rec makecluster (l: (int*int*int) list) : (int*int*int) list= // gives as an output the list of edges that are have not been processed
    let (i,j,c)= l.Head
    if (c>2) then l
             else
                 if (p.union_by_rank(i,j)=true) then union_count<-union_count+1
                 makecluster l.Tail

// here we use a non exhaustive list of distances

let output_list = makecluster Hdistance_list 

p.print()

printfn "%A" output_list

//among the remaining edges, find the first one (with minimum cost as the list is sorted) between two different groups

let rec spacing (l:(int*int*int) list) : int = 
    let (i,j,c) = l.Head
    let x = p.find_compress(i)
    let y = p.find_compress(j)
    if (x<>y) then c else spacing (l.Tail)

let res = spacing output_list
printfn "res = %A" res

let A = (p.output() |> Array.map snd |> Seq.distinct |> Seq.length) - 1 // need to remove 0

stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds
Console.ReadKey() |> ignore


      