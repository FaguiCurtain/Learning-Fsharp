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
let n_nodes = 200000
let x = y.[0..n_nodes]

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

let powers_of_2 = [|1;2;4;8;16;32;64;128;256;512;1024;2048;4096;8192;16384;32768;65536;131072;262144;524288;1048576;2097152;4194304;8388608;16777216|]

let power2 k = 
     if ((k >= 24) || (k<0)) then raise (InnerError("power exponent not allowed"))
        else powers_of_2.[k]

let convert_bitarray_to_int bitarray : int = //converts a boolean vector of size (16 for example) into an integer from 0 to 65535
    Array.fold (fun (acc,k) (bit:bool) -> let x = System.Convert.ToInt32(bit)
                                          (acc+ x*(power2 k),k+1))
               (0,0) bitarray
    |> fst
// note: we read bits from 0 and left to right so this is *not* the "natural" way to do it


let AddtoDict (dict:Dictionary<int,int list>) (k,v) =
    if not(dict.ContainsKey k) then 
                                 let value = [v]
                                 dict.Add(k,value)
                               else
                                 let v'= dict.[k]
                                 if (List.exists (fun s -> s=v) v') then ()
                                                                    else dict.[k] <-  v::v' // dictionary is mutable
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
                          else  Array.map2 (fun b1 b2 -> b1 = b2) u v 
                                |> Array.fold (fun acc x -> match x with 
                                                              | true  -> acc
                                                              | false -> acc+1) 0
let get_hammond_distance i j =
    hammond_distance edges.[i] edges.[j]

//////////////////////////////////////////

let inGraph = Array.create 16777216 false //2^24 = 16777216

// let inGraphList : int list [] = [|for i in 0L..16777215L do yield []|]
// not good because we don't know the size of the list and we run out of memory
 
let labels = [|for i in 0..n_nodes do yield convert_bitarray_to_int edges.[i] |]
let inGraphDict = new Dictionary<int,int list>() //don't forget brackets

for i in 1..n_nodes do
    let label =  labels.[i]
    inGraph.[label] <-true
    AddtoDict inGraphDict (label,i)
//    inGraphList.[label]<- i::inGraphList.[label] 



let make_neighbours_1 (x:bool[]) = // labels des voisins à distance 1
    let a = Array.create 24 0  
    let helper i = 
        let y = Array.copy x
        y.[i] <- not(x.[i])
        a.[i] <- convert_bitarray_to_int y
    for i in 0..23 do helper i                     
    a // renvoie int[0..23] labels à dist=1

let make_neighbours_2 (x:bool[]) = // voisins à distance 1
    let a = Array.create 276 0  
    // let y = Array.create 24 false
    // let reinit () =  for i in 0..23 do y.[i]<-x.[i]    
    let mutable j = 0
    let mutable k = 1
    let helper i = 
        let y= Array.copy x // est-ce que ça va plus vite ainsi ?
        y.[j] <- not (x.[j])
        y.[k] <- not (x.[k])
        a.[i] <- convert_bitarray_to_int y
        if (k=23) then j<-j+1
                       k<-j+1
    for i in 0..275 do     
        helper i               
    a // renvoie int[0..275] labels à dist=2

let check_for_neighbours_0 i = //int list
     let label =  labels.[i]
     inGraphDict.[label] |> List.filter (fun s-> not(s=i))


let check_for_neighbours_1 i = //int []
     let node = edges.[i]
     let neighbours1 = make_neighbours_1 node
     // let folder1 = fun (acc:int) node -> if inGraph.[node]=true then (acc + 1) else acc
     // Array.fold folder1 0 neighbours1
     let process_list = neighbours1 |> Array.filter (fun node -> inGraph.[node]=true)
     let folder = (fun (listacc:int list) label -> List.concat (seq[listacc; inGraphDict.[label]]) )
     Array.fold folder [] process_list

// check_for_neighbours_1 124234;;
// val it : int [] = [|667119; 667107|] // avant modif, je renvoyais des labels au lieu des nodes
// inGraphDict.[667119];;
// val it : int list = [150679; 150679] // there should be only one entry
// > inGraphDict.[667107];;
// val it : int list = [196239; 196239]
// > check_for_neighbours_1 124234;;
// val it : int list = [150679; 150679; 196239; 196239]

let check_for_neighbours_2 i =
     let node = edges.[i]
     let neighbours2 = make_neighbours_2 node
     let process_list = neighbours2 |> Array.filter (fun node -> inGraph.[node]=true)
     let folder = (fun (listacc:int list) label -> List.concat (seq[listacc; inGraphDict.[label]]) )
     Array.fold folder [] process_list

// example of how to recurse on an index and then on a list to create a list
let check_dupli listacc i = 
    let checklist = (check_for_neighbours_0 i)
    let rec helper (l_acc: (int*int*int) list) (l:int list): (int*int*int) list= 
        match l with 
          | [] -> l_acc
          | h::t -> helper ((i,h,0)::l_acc) t    
    List.append listacc (helper [] checklist) 

let dupli_list  =
     let rec main_helper listacc i =
         if (i=n_nodes) then listacc
                        else main_helper (check_dupli listacc i) (i+1)            
     main_helper [] 1
     |> List.map (fun (a,b,c) -> if (a<b) then (a,b,c) else (b,a,c)) |> List.distinct

// dupli_list |> List.length
// val it : int = 1215 ok
                  
let add_neighbours_1 listacc i =
    let addlist = check_for_neighbours_1 i
    let rec helper (l_acc: (int*int*int) list) (l:int list): (int*int*int) list= 
        match l with 
          | [] -> l_acc
          | h::t -> helper ((i,h,1)::l_acc) t    
    List.append listacc (helper [] addlist)

let add_neighbours_2 listacc i =
    let addlist = check_for_neighbours_2 i
    let rec helper (l_acc: (int*int*int) list) (l:int list): (int*int*int) list= 
        match l with 
          | [] -> l_acc
          | h::t -> helper ((i,h,2)::l_acc) t    
    List.append listacc (helper [] addlist)

let neighbours_1_list  =
     let rec main_helper listacc i =
         if (i=n_nodes) then listacc
                        else main_helper (add_neighbours_1 listacc i) (i+1)            
     main_helper [] 1
     |> List.filter (fun (a,b,c)-> (a<b))

let neighbours_2_list  =
     let rec main_helper listacc i =
         if (i=n_nodes) then listacc
                        else main_helper (add_neighbours_2 listacc i) (i+1)            
     main_helper [] 1
     |> List.filter (fun (a,b,c)-> (a<b))

let n_dupli = dupli_list |> List.length
let n1 = neighbours_1_list |> List.length
let n2 = neighbours_2_list |> List.length

printfn "dupli = %A" n_dupli // 1215 ok
printfn "edges 1 = %A" n1 //28594 ok
printfn "edges 2 = %A" n2 // 339480 not ok (328982 correct)

let process_list = List.concat (seq[dupli_list;neighbours_1_list;neighbours_2_list])
//
//                 
////////////////////////////////////////////

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


stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds
Console.ReadKey() |> ignore

// wrong answer runs in 679s
      