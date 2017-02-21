// Optimal Balanced Search Tree
// given search results and their probabilities,
// build the tree with the smallest expected time search where time is proportional to the depth of the searched item

// Subproblems :
// consider the subproblem Cij composed of only consecutive items i...j , and order them by size
// s = j-i

// Consider an instance of the optimal binary search tree problem with 7 keys (say 1,2,3,4,5,6,7 in sorted order) and frequencies w1=.05,w2=.4,w3=.08,w4=.04,w5=.1,w6=.1,w7=.23. 
// What is the minimum-possible average search time of a binary search tree with these keys?

open System

let num_items = 7 // number of search items
let limit = 9999999.0

// let p =[|0.0;0.05;0.4;0.08;0.04;0.1;0.1;0.23|]
let p =[|0.0;0.2;0.05;0.17;0.1;0.2;0.03;0.25|]


let A =  Array2D.create (num_items+1) (num_items+1) 0.0
let mutable tmp = 0.0


let getA i j = 
    if ((i<=j) && (j<=num_items) && (i<=num_items)) then A.[i,j]
    else 0.0
   
for s in 0..(num_items-1) do
    for i in 1..(num_items-s) do
        tmp <- limit
        for r in i..(i+s) do
            printfn "s=%A i=%A r=%A" s i r
            tmp<- min tmp ((getA i (r-1))+(getA (r+1) (i+s)))
        A.[i,i+s] <- tmp + Array.sum (p.[i..i+s])

printfn "A=%A" A
