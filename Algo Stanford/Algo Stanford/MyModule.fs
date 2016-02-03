namespace MyModule

   module Dict =
      open System.Collections.Generic
      let toSeq d = d |> Seq.map (fun (KeyValue(k,v)) -> (k,v))
      let toArray (d:IDictionary<_,_>) = d |> toSeq |> Seq.toArray
      let toList (d:IDictionary<_,_>) = d |> toSeq |> Seq.toList
      let ofMap (m:Map<'k,'v>) = new Dictionary<'k,'v>(m)  :> IDictionary<'k,'v>
      let ofList (l:('k * 'v) list) = new Dictionary<'k,'v>(l |> Map.ofList) :> IDictionary<'k,'v>
      let ofSeq (s:('k * 'v) seq) = new Dictionary<'k,'v>(s |> Map.ofSeq) :> IDictionary<'k,'v>
      let ofArray (a:('k * 'v) []) = new Dictionary<'k,'v>(a |> Map.ofArray) :> IDictionary<'k,'v>

   module Stack =
      type 'a stack =
        | EmptyStack
        | StackNode of 'a * 'a stack
        
      let hd = function
          | EmptyStack -> failwith "Empty stack"
          | StackNode(hd, tl) -> hd
        
      let tl = function
          | EmptyStack -> failwith "Emtpy stack"
          | StackNode(hd, tl) -> tl
        
      let cons hd tl = StackNode(hd, tl)
    
      let empty = EmptyStack
    
      let rec update index value s =
          match index, s with
            | index, EmptyStack -> failwith "Index out of range"
            | 0, StackNode(hd, tl) -> StackNode(value, tl)
            | n, StackNode(hd, tl) -> StackNode(hd, update (index - 1) value tl)
        
      let rec append x y =
          match x with
            | EmptyStack -> y
            | StackNode(hd, tl) -> StackNode(hd, append tl y)

      let rec map f = function
            | EmptyStack -> EmptyStack
            | StackNode(hd, tl) -> StackNode(f hd, map f tl)
        
      let rec rev s =
          let rec loop acc = function
               | EmptyStack -> acc
               | StackNode(hd, tl) -> loop (StackNode(hd, acc)) tl
          loop EmptyStack s

      let rec contains x = function
           | EmptyStack -> false
           | StackNode(hd, tl) -> hd = x || contains x tl
        
      let rec fold f seed = function
           | EmptyStack -> seed
           | StackNode(hd, tl) -> fold f (f seed hd) tl