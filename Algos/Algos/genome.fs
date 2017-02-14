///////////////// preparing the data ////////////////////

open System
open System.Collections.Generic
open System.IO

let ABOgene= "ggccgcctcccgcgcccctctgtcccctcccgtgttcggcctcgggaagtcggggcggcgggcggcgcgggccgggaggggtcgcctcgggctcaccccgccccagggccgccgggcggaaggcggaggccgagaccagacgcggagccatggccgaggtgttgcggacgctggccg"

let dict = new Dictionary<string,int list>()

let len = ABOgene.Length

let insert_or_update s x = 
    if dict.ContainsKey s then dict.[s]<- x::dict.[s]
                          else dict.Add(s,[x])

for i in 0..(len-7) do insert_or_update ABOgene.[i..i+6] i
   
                    
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




