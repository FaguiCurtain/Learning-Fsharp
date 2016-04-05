let rec binomial n (k:int) = 
       match k with
         | 0 -> 1
         | _ -> n * (binomial (n-1) (k-1) ) / k
 
let rec binomial1 n (k:int) (acc : (int * int)) =
     let work xx a b = xx * a / b
     let lmul,rdiv = acc
     let x = binomial1 (n-1) (k-1)
     match k with
         | 0 -> lmul / rdiv
         | _ -> binomial1 (n-1) (k-1) (lmul*n,k*rdiv)

     // printfn "%d %d" lmul rdiv

//call for example with binomial1 10 5 (1,1) 

// descente récursive
let binomial_iter n k = 
    let mutable lmul = 1
    let mutable rdiv = 1
    let mutable nn = n
    let mutable kk = k
    let x = (nn,kk,lmul,rdiv)
    while (kk>0) do 
                  lmul <- lmul * nn
                  rdiv <- rdiv * kk
                  nn <- nn-1
                  kk <- kk-1              
    (lmul / rdiv) |> printfn "%d" |> ignore

// récursion à l'envers
// possible car la fonction s'inverse facilement

let rec binomial2 n k =
      let mutable acc = 1
      let mutable nn = n-k
      let mutable kk = 0
      
      for i in 1..k do
               
             nn <- nn+1
             kk  <- kk+1
             acc <- acc * nn/ kk    
      acc
  

