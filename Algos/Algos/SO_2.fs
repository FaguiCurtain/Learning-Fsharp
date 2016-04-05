module Dict = 
    open System.Collections.Generic
    let ofMap (m:Map<'k,'v>) = new Dictionary<'k,'v>(m)  :> IDictionary<'k,'v>

open Dict
let mymap = Map.ofList [(1,true);(2,false)]
let dict = ofMap mymap

printfn "%A" dict.[1]

System.Console.ReadKey() |> ignore