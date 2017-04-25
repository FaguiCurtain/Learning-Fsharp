open System
open System.IO
open System.Net

open FSharp.Data
open XPlot.GoogleCharts

open Deedle

/// converts a date in UNIX POSIX format into DateTime
let toDateTime (timestamp:int) =
    let start = DateTime(1970,1,1,0,0,0,DateTimeKind.Utc)
    start.AddSeconds(float timestamp).ToLocalTime()

// let unix_now = DateTimeOffset(DateTime.Now).ToUnixTimeSeconds() //val unix : int64 = 1488254491L
//let dt =  DateTimeOffset.FromUnixTimeSeconds(unix)  //val dt : DateTimeOffset = 2017/02/28 4:01:31 +00:00
//dt.LocalDateTime 

let ToUnixTimeSeconds (d:DateTime) : int64 = 
    let start = DateTime(1970,1,1,0,0,0,DateTimeKind.Utc)
    int64 (d - start).TotalSeconds

let unix_now = ToUnixTimeSeconds DateTime.UtcNow
let num_candles = 30
let start_date1 = string (unix_now - int64 (num_candles * 300))



/// Get the contents of the URL via a web request
let http (url: string) =
    let req = WebRequest.Create(url)
    let resp = req.GetResponse()
    let stream = resp.GetResponseStream()
    let reader = new StreamReader(stream)
    let html = reader.ReadToEnd()
    resp.Close()
    html
  

let CallAPI_and_save_JSON (webAPIcall:string,jsonFilename:string) = 
    let s = http webAPIcall
    File.WriteAllText (jsonFilename,s)

  

let jsonFilename = "/Users/francois-guillaume.rideau/Documents/test.json"
// type BookOrder = JsonProvider<jsonFilename> doesn't work, why ??

type BookOrder = JsonProvider<"/Users/francois-guillaume.rideau/Documents/order_book.json">

type ChartData = JsonProvider<"/Users/francois-guillaume.rideau/Documents/chart_data.json">


// Public API method to get Order Book
// https://poloniex.com/public?command=returnOrderBook&currencyPair=BTC_NXT&depth=10
let commandType = "returnOrderBook"
let currencyPair = "BTC_ETH"
let depth = string 10
let polReq = "https://poloniex.com/public?" + "command=" + commandType + "&currencyPair=" + currencyPair + "&depth=" + depth

// Returns candlestick chart data. 
// Required GET parameters are "currencyPair", "period" (candlestick period in seconds; valid values are 300, 900, 1800, 7200, 14400, and 86400), 
// "start", and "end". "Start" and "end" are given in UNIX timestamp format and used to specify the date range for the data returned. 
// https://poloniex.com/public?command=returnChartData&currencyPair=BTC_XMR&start=1405699200&end=9999999999&period=14400

// 1405699200 = 19 july 2014


let commandType1 = "returnChartData"
let currencyPair1 = "BTC_XMR"
// let start_date1 = string 1405699200M // already defined up there
let end_date1   = string 9999999999M
let period1     = string 300M
let polReq1 = "https://poloniex.com/public?" + "command=" + commandType1 + "&currencyPair=" + currencyPair1 + "&start=" + start_date1
              + "&end=" + end_date1 + "&period=" + period1

    




type Fill = 
    | TotalFill   of decimal * decimal
    | PartialFill of decimal * decimal 

let executable_price (bookOrder:decimal [][]) (size:decimal) : Fill =
    let depth = Array.length bookOrder

    let rec partial_fill (bookOrder_l:decimal [] list) ((size_left,avg_price):decimal*decimal)=
        match bookOrder_l with
          | [] -> PartialFill (size - size_left,avg_price)
          | _  -> let best_order = List.head bookOrder_l
                  let (p,s) = (best_order.[0], best_order.[1])
                  if (s>=size_left) then TotalFill (size, (avg_price * (size-size_left) + p * size_left) / size)
                      else partial_fill (List.tail bookOrder_l) (size_left-s,(avg_price * (size-size_left) + p * s) / (size-size_left+s))
    partial_fill (Array.toList bookOrder) (size,0M)

let myBookOrder = [| [|1M;10M|];[|2M;10M|];[|3M;20M|] |]







[<EntryPoint>]
let main args =
    printfn "Arguments passed to function : %A" args

    printfn "hello world"

    // save a template once for all
    CallAPI_and_save_JSON (polReq ,"/Users/francois-guillaume.rideau/Documents/order_book.json")
    CallAPI_and_save_JSON (polReq1,"/Users/francois-guillaume.rideau/Documents/chart_data.json")

    // let BookOrderAsync = BookOrder.AsyncLoad(polReq) // comment ça marche ???
    let BookOrderNow = BookOrder.Load(polReq)
    // BookOrderNow.Bids // ça marche directement
    let bids = BookOrderNow.Bids

    (* BookOrderNow.Bids example for BTCETH
 
    val it: decimal [] []
    [|   [|0.04174999M; 1.899076M|]; [|0.04175000M; 1.9M|];
         [|0.04175100M; 7.49640255M|]; [|0.04175200M; 10M|];
         [|0.04177898M; 30.372M|]; [|0.04177900M; 69.9284M|];
         [|0.04178498M; 0.00616378M|]; [|0.04178899M; 185.21624954M|];
         [|0.04178900M; 143.203M|]; [|0.04179970M; 2M|]|]

    the best offer is at the price of 1 ETH = 0.04174999 BTC for an amount of 1.899076 ETH *)
   
    BookOrderNow.JsonValue.Properties()
       |> Seq.map fst
       |> printfn "%A" 

       //=> seq ["asks"; "bids"; "isFrozen"; "seq"]

    // this function uses x.Bids which is a bit stupid...
    let getJsonContent1 (x:BookOrder.Root) =
        function
          |"bids" -> x.Bids
          |"asks" -> x.Asks
          |_ -> failwith ".."
    
    let getJsonContent (x:BookOrder.Root) varname =
        x.JsonValue.GetProperty varname
          |> JsonExtensions.AsArray
          |> Array.map
               (JsonExtensions.AsArray >> Array.map (JsonExtensions.AsDecimal))

    
    printfn "%A %A %A" (executable_price myBookOrder 20M)(executable_price myBookOrder 30M)(executable_price myBookOrder 50M)



    let ChartNow = ChartData.Load(polReq1)

    // too much data to visualize
    // [for x in ChartNow do yield (x.Date,x.Low,x.Open,x.Close,x.High)]

    let l = ChartNow.Length

    [for i in (l-19)..(l-1)   do let x = ChartNow.[i]
                                 yield (x.Date,x.Low,x.Open,x.Close,x.High)
                          ]
               |> Chart.Candlestick |> ignore



    0
   
