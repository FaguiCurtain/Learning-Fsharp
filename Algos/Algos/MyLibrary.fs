// useful functions

// returns the minimum + index of the minimum
let mini (s : (int*int) list) = 
         match s with 
            | [] -> (-1,(-1,-1))
            | _  -> s |> Seq.mapi (fun i x -> (i, x)) |> Seq.minBy snd

let maxi (s : (int*int) list) = 
         match s with 
            | [] -> (-1,(-1,-1))
            | _  -> s |> Seq.mapi (fun i x -> (i, x)) |> Seq.maxBy snd
