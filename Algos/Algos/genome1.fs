///////////////// preparing the data ////////////////////

open System
open System.Collections.Generic
open System.IO

let A = [|for i in 1..16 do yield ""|]
A.[1] <-  "gtctctcggtaggcctcttggcagctctatcggcgagtatctcggcacg"
A.[2] <-  "gtctcgtgacaggtatctcggtaactatctcggtagctaacgcggcgtg"
A.[3] <-  "gtcactcggtaggcctctcggtgagtatctcgataggtaactcggcgtc"
A.[4] <-  "atctcgcggtagccaacttggtaggtctatcggcaagtctctccgcgcc"
A.[5] <-  "gtctctcggcaggtatattcataggtctattgataggtcacttggcatg"
A.[6] <-  "gtcacgccgtaggtaacttcgtaggtctatcggtaggtctcgtgacatg"
A.[7] <-  "gcctatcggtgaccaactcgatgggtctatcggcaagccacgcgatgtg"
A.[8] <-  "gtctcttcgtagctatcttcataggtcacgcgatgggtcacgcggtgtc"
A.[9] <-  "gtatctcggtaggcatctcggtagctctatccgtaagtatctcgatgtc"
A.[10] <- "atatcttggcaaccaactcgatgggtctatcggcaagccacgcgatatg"
A.[11] <- "gtctctcggtaagtatctccgcgagtcaatcgacgggtctatcgacatc"
A.[12] <- "atctctcggtaggcctctcggtgagtatctcgataggtctatcggtatg"
A.[13] <- "gtcacgcgatagctctctcggtgactctcttggcaggtctatccacgtc"
A.[14] <- "gcctctcggtaggcctctcggcaggtcaattggtgggtctctcgatatc"
A.[15] <- "gtctcgcgataggtatctcgatgggtctcttgatgggtctctccgcatg"

let insert_or_update (dict:Dictionary<string,int list>) s x = 
    if dict.ContainsKey s then dict.[s]<- x::dict.[s]
                          else dict.Add(s,[x])
   
type genindex = Dictionary<string,int list>
let AA = [|for i in 0..16 do yield (new Dictionary<string,int list>())|] 

let len = 49
for k in 1..15 do                          
    for i in 0..(len-7) do insert_or_update AA.[k] A.[k].[i..i+6] i

let compare_gene i j =
    let dico1 = AA.[i]
    let dico2 = AA.[j]
    let keys2 = dico2.Keys
    let candi = keys2 |> Seq.toList |> List.filter (fun s -> (dico1.ContainsKey s))
    candi |> List.map (fun s -> ( (snd (dico1.TryGetValue(s)) )|> List.min))
    


                    
let getpos (read:string) = 
    printfn "read= %A" read
    let readleft = read.[0..6]
    let readright = read.[7..] 
    let resleft = dict.TryGetValue(readleft)
    let resright = dict.TryGetValue(readright)
    if ((fst resleft) = true)  then 
                                   let l = (snd resleft)                                   
                                   let x = l.Head
                                   printfn "ABO = %A" ABOgene.[x..(x+13)]
                                   printfn "left= %A" l
    if ((fst resright) = true) then 
                                   let r = (snd resright) |> List.map (fun s -> (s-7))           
                                   let x = r.Head
                                   printfn "ABO = %A" ABOgene.[x..(x+13)]
                                   printfn "right = %A" r
    printfn ""
  
getpos("ccggcctcgggaag")  
getpos("ttgcggacgctagc")
getpos("tcgggctccccccg")
getpos("ggggggaaggcgga")
getpos("tctgtccccccccg")


let dictInnerMerge (dict1:Dictionary<'a,'b>) (dict2:Dictionary<'a,'b>) (appender:'b->'b->'c) = 
    let dict3 = new Dictionary<'a,'c list>()
    dict1 |> Seq.iter (fun (KeyValue(key1,v1)) ->
                          match dict2.TryGetValue(key1) with
                            | (true,v2)  -> let x = appender v1 v2
                                            if dict3.ContainsKey key1 then dict3.[key1]<- x::dict3.[key1]
                                                                      else dict3.Add(key1,[x])
                            | (false,v2) -> ()
                      )
    dict3

let maxsim (gene1:string) (gene2:string) i = 
    let mutable ok = true
    if gene1.[i..i+19]<>gene2.[i..i+19] then (-1 ,-1)
       else let mutable j = 19
            while ok do j<- j+1
                        if (gene1.[j]<>gene2.[j]) then ok <- false else ok <- (j<=47)
            (i+1,j)
            
let dict3 = dictInnerMerge AA.[3] AA.[12] (fun s1 s2 -> (s1,s2));;
