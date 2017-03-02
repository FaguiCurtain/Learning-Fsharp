Open FSharp.Data

let value = JsonValue.Load("/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/ETHBTC_Poloniex.json")

match value with

  | JsonValue.Array [| data |] ->
       for record in data do
           if record?close <> JsonValue.Null then
              printfn "%A" record?date

  | _ -> printfn "failed"


 type ETHBTC = JsonProvider<"/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/ETHBTC_Poloniex.json">

 let doc = ETHBTC.GetSamples()

 [for item in doc do yield (toDateTime item.Date).Date , item.Close]


let rawfile = CsvFile.Load("/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/coindesk-bpi-USD-close_data-2015-09-01_2017-02-28.csv")

for row in rawfile.Rows do
       printfn "%A %A" (System.DateTime.Parse (row.GetColumn("Date"))) (float (row.GetColumn("Close Price")))
        
       // price_table.[BTC].Add(System.DateTime.Parse (row.GetColumn("Date")), float (row.GetColumn("Close Price")) )

//for row in rawfile.Rows do
//       let d = System.DateTime.Parse (row.GetColumn("Date"))
//       let p = float (row.GetColumn("Close Price"))
//       printfn "%A %A" d p
//       price_table.[BTC].Add (d,p)

exception MyFSharpError1 of string

try
    failwith "fail"
with
    | Failure msg -> "caught: " + msg
    | MyFSharpError1 msg -> " MyFSharpError1: " + msg
    | :? System.InvalidOperationException as ex -> "unexpected"     

for row in rawfile.Rows do
    try
       printfn "%A %A" (System.DateTime.Parse (row.GetColumn("Date"))) (float (row.GetColumn("Close Price")))     
    with 
       | MyFSharpError1 msg ->    " MyFSharpError1: " + msg |> ignore
       | :? System.Exception as ex ->  "parsing error"  |> ignore                


let x = File.ReadAllLines "/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/coindesk-bpi-USD-close_data-2015-09-01_2017-02-28.csv"

let signature_size = 3
let l = x.Length

let split (text:string)=
    text.Split [|'\t';' ';','|]

let ParseLine s =
    s |> split |> (fun res -> (System.DateTime.Parse res.[0].[1..]).Date,float res.[2])


                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               