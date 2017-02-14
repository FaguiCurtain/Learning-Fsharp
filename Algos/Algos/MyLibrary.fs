// useful functions

// returns the minimum + index of the minimum
namespace MyLibrary

namespace MyLibrary.MyUsefulFunctions

exception InnerError of string
module Search =
   let mini (s : (int*int) list) = 
         match s with 
            | [] -> (-1,(-1,-1))
            | _  -> s |> Seq.mapi (fun i x -> (i, x)) |> Seq.minBy snd

   let maxi (s : (int*int) list) = 
         match s with 
            | [] -> (-1,(-1,-1))
            | _  -> s |> Seq.mapi (fun i x -> (i, x)) |> Seq.maxBy snd

module Bitwise = 
   let rec sumbits (n:int):int=
      let rec helper acc m =
         match m with
            | 0 -> acc
            | 1 -> acc+1 // enlever cela ?
            | _ -> let r = m%2
                   helper (acc+r) (m>>>1)
      helper 0 n

   let power2 k = 
     let powers_of_2 = [|1;2;4;8;16;32;64;128;256;512;1024;2048;4096;8192;16384;32768;65536;131072;262144;524288;1048576;2097152;4194304;8388608;16777216|]
     if ((k >= 25) || (k<0)) then raise (InnerError("power exponent not allowed"))
        else powers_of_2.[k]

    

namespace MyLibrary.MyCollections
module UnionFind = 
   type Node = { Parent:int ; Rank:int}

   type Partition (N:int) = 
      
        let nodearray =   [| for i in 0..N do yield {Parent = i; Rank = 0} |]

        let length = (Array.length nodearray) - 1

        member p.Item 
            with get(idx) = nodearray.[idx]

        member p.find (i) = 
            let mutable tmp0 = i
            let mutable tmp1 = p.[i].Parent
            while (tmp0 <> tmp1) do tmp0 <- tmp1
                                    tmp1 <- p.[tmp1].Parent
            tmp0
        member p.find_compress (i) :int =
            let mutable tmp0 = i
            let mutable tmp1 = p.[i].Parent
            let rec one_step ok list =
                match ok with 
                  | false -> list
                  | true ->   tmp0 <- tmp1
                              tmp1 <- p.[tmp1].Parent
                              one_step (tmp0<>tmp1) (tmp0::list) 
        
            let list = one_step (tmp0<>tmp1) [i]  
            // ranks stay the same with find_compress
            list |> List.head |> (fun i ->  let r = nodearray.[i].Rank
                                            (nodearray.[i] <- {Parent = tmp0 ; Rank=r} ))
            list |> List.tail |> List.iter ( fun i -> let r = nodearray.[i].Rank
                                                      (nodearray.[i] <- {Parent = tmp0 ; Rank=r} ))
        
            tmp0

        member p.union_by_rank (i,j) : bool = // returns false if i and j hav the same parent, true otherwise
           if i=j then false else
              let x = p.find_compress(i)
              let y = p.find_compress(j)
              let rx = p.[x].Rank 
              let ry = p.[y].Rank
          
              if x=y then false else
                 if (rx<ry) then nodearray.[x] <- {Parent = y; Rank = rx}                         
                 if (ry<rx) then nodearray.[y] <- {Parent = x; Rank = ry}                
                 if (rx=ry) then nodearray.[x] <- {Parent = y; Rank = ry}
                                 nodearray.[y] <- {Parent = y; Rank = ry+1}
                 true                             

        member p.print() = 
           printfn "%A" nodearray
  
        member p.output() = [|for i in 0..length do yield (i,p.find_compress(i))|]


namespace Graph 
module graph = 

    // kmeans clustering algorithm for a list of edges and cost (= weighted graph)
    let kClustering (edges:(int*int*int) list)(num_clusters:int)= //returns (partition,spacing)
       // format of edges: (vertex1,vertex2, cost) where vertex1 and vertex2 are the summits of the corresponding edge


       // let n_nodes
        let l1 = edges |> List.map (fun (a,b,c)->a) 
        let l2 = edges |> List.map (fun (a,b,c)->b)
        let n_nodes = List.append l1 l2 |> List.distinct |> List.length

        let num_step = n_nodes-num_clusters

        let p = MyLibrary.MyCollections.UnionFind.Partition(n_nodes)

        let mutable union_count = 0

        let rec makecluster (l: (int*int*int) list) : (int*int*int) list= // gives as an output the list of edges that are have not been processed
            if (union_count = num_step) then l
                  else
                     let (i,j,c)= l.Head
                     if (p.union_by_rank(i,j)=true) then union_count<-union_count+1
                     makecluster l.Tail

        let output_list = makecluster edges 

        p.print()

        //among the remaining edges, find the first one (with minimum cost as the list is sorted) between two different groups

        let rec spacing (l:(int*int*int) list) : int = 
            let (i,j,c) = l.Head
            let x = p.find_compress(i)
            let y = p.find_compress(j)
            if (x<>y) then c else spacing (l.Tail)

        let res = spacing output_list
        printfn "spacing = %A" res

        (p,res) //output of kclustering



