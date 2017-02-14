[<CustomComparison>]
type antiint = int
     interface System.IComparable with
       member x.CompareTo yobj =
           match yobj with
             | :? MyThing as y -> compare x.Stamp y.Stamp
             | _ -> invalidArg "yobj" "cannot compare values of different types"