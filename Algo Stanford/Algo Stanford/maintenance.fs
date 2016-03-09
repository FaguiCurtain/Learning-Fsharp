///////////////// preparing the data ////////////////////

open System
open System.Collections.Generic
open System.IO

open FSharp.Core

open FSharpx.Collections

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

let x = File.ReadAllLines "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\PA6 - Median.txt"
let list = x |> Array.map (fun s -> int s) |> Array.toList

//let lowheap  = new PriorityQueue<int,int>() // support extract max
//
//let highheap = new PriorityQueue<int,int>() // support extract min 



// let lowheap = lowheap.Insert(1) is OK in F# interactive but not in compiled code

//Testcase B:
//{11 3 6 9 2 8 4 10 1 12 7 5}
//
//Program output:
//MEDIANS:
//11 3 6 6 6 6 6 6 6 6 
//7 6 
//
//LOW HEAP:
//1 2 3 4 5 6 
//
//HIGH HEAP:
//7 8 9 10 11 12 
//medians sum= 75

// let list = [11;3;6;9;2;8;4;10;1;12;7;5]
// answer = 75

// let list = [1;2;4;3;5;6;8;7;9]
// answer = 25

let a0 = list.[0]
let a1 = list.[1]
let amin = min a0 a1
let amax = max a0 a1

let N = List.length list

let median_array = [|for i in 0..N do yield 0|]
median_array.[1]<-a0
median_array.[2]<-amin

let list1 = list |> List.tail |> List.tail

//let lowheap = new Heap<int>(true,0,E)   // creates empty Heap, supporting extract max
//let highheap = new Heap<int>(false,0,E) // creates empty Heap, supporting extract min

let lowheap  = new Heap<int>(true,1,T(amin,[E]))
let highheap = new Heap<int>(false,1,T(amax,[E]))

let rec onestep j list (isCountOdd:bool) (lowheap:Heap<int>) (highheap:Heap<int>) =
    // printfn "lowheap = %A" (lowheap |> Seq.toList)
    // printfn "highheap = %A" (highheap |> Seq.toList)

    match list with
      | [] -> (E,E)
      | x::t -> 
                let a = lowheap.Head
                let b = highheap.Head

                if isCountOdd = false then
                                          if (x<=a) then let lowheap1 = lowheap.Insert x
                                                         let highheap1 = highheap
                                                         median_array.[j]<- a  
                                                         onestep (j+1) t true lowheap1 highheap1                                      
                                                    else if (x>=b) then let highheap2 = highheap.Tail()
                                                                        let highheap1 = highheap2.Insert x
                                                                        let lowheap1 = lowheap.Insert b
                                                                        median_array.[j] <- b
                                                                        onestep (j+1) t true lowheap1 highheap1   
                                                                   else let lowheap1  = lowheap.Insert x
                                                                        let highheap1 = highheap
                                                                        median_array.[j]  <- x
                                                                        onestep (j+1) t true lowheap1 highheap1  
                                      else // CountOdd = true
                                          if (x<=a) then let lowheap2 = lowheap.Tail()
                                                         let lowheap1 = lowheap2.Insert x
                                                         let highheap1 = highheap.Insert a                                 
                                                         median_array.[j]<- lowheap1.Head
                                                         onestep (j+1) t false lowheap1 highheap1 
                                                         else if (x>=b) then let highheap1 = highheap.Insert x
                                                                             let lowheap1 = lowheap
                                                                             median_array.[j] <- lowheap.Head
                                                                             onestep (j+1) t false lowheap1 highheap1 
                                                                        else let highheap1 = highheap.Insert x
                                                                             let lowheap1 = lowheap
                                                                             median_array.[j] <- lowheap.Head
                                                                             onestep (j+1) t false lowheap1 highheap1  

onestep 3 list1 false lowheap highheap |> ignore

printfn "median_array = %A" median_array
printfn "median_array sum = %A" (median_array |> Array.sum)
stopWatch.Stop()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds
Console.ReadKey() |> ignore

// correct answer = 1213

