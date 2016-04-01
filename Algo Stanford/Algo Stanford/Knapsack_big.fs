///// The Knapsack problem: pack the most valuable items with a volume constraint /////

open System
open System.Collections
open System.Collections.Generic
open System.IO

open FSharp.Core


let stopWatch = System.Diagnostics.Stopwatch.StartNew()

///////////////// preparing the data /////////////////
// format of the files
//[knapsack_size][number_of_items]
//[value_1] [weight_1]
//[value_2] [weight_2]

//let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\PA3 - knapsack_big.txt"
let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\PA3 - knapsack1.txt"

let split (text:string)=
    text.Split [|'\t';' '|]

let splitIntoValues (A: 'T[]) =  
    (int A.[0],int A.[1])

let parseHeader (line:string)=
    line
    |> split
    |> Array.filter (fun s -> not(s=""))
  //  |> Array.map (fun s-> (int s))
    |> splitIntoValues

let parseLine (line:string) = 
    line
    |> split 
    |> splitIntoValues
   
let objects = x |> Array.map parseLine //  [value_1] [weight_1]
// let objects = [|(6,4);(3,4);(2,3);(4,2);(4,3)|]

let (knapsack_size,number_of_items) = objects.[0] // (2000000,2000)

// let A = Array2D.create 2001 2000001 0 // System.OutOfMemoryException
// initialize with A.[0,x] = 0 for any x

let mutable V = Array.create (knapsack_size+1) 0
let mutable V' = Array.create (knapsack_size+1) 0

let mutable weight = 0
let mutable value = 0

for i in 1..number_of_items do // doesn't work for number_of_items = 100...
    weight <- snd objects.[i]
    value <- fst objects.[i]
    for x in 0..knapsack_size do 
        // if x%10000 = 0 then printfn "%A %A" i x
        
        if x >= weight then  
           V'.[x]<-max V.[x] ( V.[(x - weight) ] + value )
                                  else
           V'.[x]<- V.[x]
    V <- Array.copy V'
    // printfn "V'= %A" V'

printfn "answer = %A" V.[knapsack_size] 
//answer = 2493893
//val it : unit = ()

printfn "%f" stopWatch.Elapsed.TotalMilliseconds
Console.ReadKey() |> ignore

let stopWatch1 = System.Diagnostics.Stopwatch.StartNew()

let AddtoDictL (dict:Dictionary<int*int,int list>) (k,v) =
    if not(dict.ContainsKey k) then 
                              let value = [v]
                              dict.Add(k,value)
                            else
                               let v'= dict.[k]
                               dict.[k] <-  v::v' // dictionary is mutable

let Add_or_CheckDict (dict:Dictionary<int*int,int>) (k,v) =
    if not(dict.ContainsKey k) then 
                              let value = v
                              dict.Add(k,value)
                            else
                               let v'= dict.[k]
                               if (v'<>v) then failwith("key error") // dictionary is mutable

type knapsack (size:int,object_list:(int*int)[]) = 
    let answer_weight_cache = new Dictionary<int*int,int>()
    let answer_list_cache = new Dictionary<int*int,int list>()
    let knapsack_size = size
    let objects = object_list //  [value_1] [weight_1]
    
    member this.compute(maxweight:int,number_obj:int)=
       let rec helper x i =
           match i with
             | 0 -> 0
             | _ -> let cached_answer = ref -1
                    let in_cache = answer_weight_cache.TryGetValue ((maxweight,number_obj),cached_answer)
                    match in_cache with
                      | true -> !cached_answer
                      | false ->   let weight = snd objects.[i]
                                   let value =  fst objects.[i]
                                   if x > weight then let ans = max (helper x (i-1)) ( (helper (x-weight) (i-1)) + value)
                                                      Add_or_CheckDict answer_weight_cache ((x,i),ans)
                                                      ans
                                                 else let ans = helper x (i-1)
                                                      Add_or_CheckDict answer_weight_cache ((x,i),ans)
                                                      ans
       helper maxweight number_obj
     
     member this.print_answer_cache()=
       let keylist = answer_weight_cache.Keys |> Seq.toList |> List.sort
       for k in keylist do 
           printfn "key (weight,n_obj) = %A ; value = %A" k answer_weight_cache.[k]  



let KS = new knapsack(knapsack_size,objects)
printfn "knapsack_size = %A number_of_items = %A" knapsack_size number_of_items

let res = KS.compute(knapsack_size,number_of_items)
// let res = KS.compute (6,4) // small example
printfn "res = %A" res
// KS.print_answer_cache()        

printfn "%f" stopWatch1.Elapsed.TotalMilliseconds
Console.ReadKey() |> ignore
