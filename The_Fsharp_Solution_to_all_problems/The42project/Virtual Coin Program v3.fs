//Virtual Coin Taxes main program

module MainProgram

open System
open System.Collections.Generic
open System.IO
open FSharp.Data


open VirtualCoinTaxes
open VirtualCoinTaxes.Definitions
open VirtualCoinTaxes.PriceTable
open VirtualCoinTaxes.MyTransactions

let debug_init_myT (myT:transaction_DB) = 

    let dt1 = System.DateTime.Parse "2016-01-13 16:19:47.606"
    let dt2 = System.DateTime.Parse "2016-02-13 16:19:47.606"
    let dt3 = System.DateTime.Parse "2016-03-13 16:19:47.606"
    let dt4 = System.DateTime.Parse "2016-04-13 16:19:47.606"

    myT.Add ("ID0001",{time=dt1;pair=(BTC,EUR);size= -100.0;price=100.0;fee=2.0 })
    myT.Add ("ID0002",{time=dt2;pair=(BTC,EUR);size=  -50.0;price=120.0;fee=1.0 })
    myT.Add ("ID0003",{time=dt3;pair=(BTC,EUR);size=  +50.0;price=200.0;fee=1.0 })
    myT.Add ("ID0004",{time=dt4;pair=(BTC,EUR);size=  +50.0;price=200.0;fee=2.0 })
    //myT.Add ("ID0005",{time=dt1;pair=(ETH,EUR);size= -100.0;price=10.0 ;fee=2.0 })
    //myT.Add ("ID0006",{time=dt2;pair=(ETH,EUR);size=  -50.0;price=12.0 ;fee=1.0 })
    //myT.Add ("ID0007",{time=dt3;pair=(ETH,EUR);size=  +50.0;price=20.0 ;fee=1.0 })
    //myT.Add ("ID0008",{time=dt4;pair=(ETH,EUR);size=  +50.0;price=20.0 ;fee=2.0 })



let debug_init_price_table (price_table:price_table)=

    let dt11=System.DateTime.Parse "2016-01-13"
    let dt12=System.DateTime.Parse "2016-02-13"
    let dt13=System.DateTime.Parse "2016-03-13"
    let dt14=System.DateTime.Parse "2016-04-13"

    price_table.[EUR].Add(dt11,100.0)
    price_table.[EUR].Add(dt12,101.0)
    price_table.[EUR].Add(dt13,110.0)
    price_table.[EUR].Add(dt14,111.0)

    price_table.[BTC].Add(dt11,10000.0)
    price_table.[BTC].Add(dt12,12120.0)
    price_table.[BTC].Add(dt13,22000.0)
    price_table.[BTC].Add(dt14,22200.0)

    price_table.[ETH].Add(dt11,1000.0)
    price_table.[ETH].Add(dt12,1212.0)
    price_table.[ETH].Add(dt13,2200.0)
    price_table.[ETH].Add(dt14,2220.0)

let KrakenOnlyinit (myT:transaction_DB) =

    let rawfile = CsvFile.Load("/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/Kraken trades.csv")

    let buy_or_sell (order)=
        match order with
          | "buy"  -> 1.0
          | "sell" -> -1.0
          | _ -> 0.0       // should not happen
    
    let ParsePair_Kraken pair =
        match pair with
          | "XDAOXETH" -> (DAO,ETH)
          | "XETHXXBT" -> (ETH,BTC)
          | "XETHZEUR" -> (ETH,EUR)
          | "XXBTZEUR" -> (BTC,EUR)
          | _ -> failwith "pair not acknowledged"

    for row in rawfile.Rows do

        myT.Add(row.GetColumn("txid"),{time=  System.DateTime.Parse (row.GetColumn("time"));
                                       pair=  ParsePair_Kraken (row.GetColumn("pair"));
                                       size=  float ( buy_or_sell(row.GetColumn("type")) * float (row.GetColumn("vol")));                                         
                                       price= float (row.GetColumn("price"));
                                       fee=   float (row.GetColumn("fee"))
                                       }    )
    |> ignore
    // printfn "%f" (myT.["TRSYD5-F2NKZ-KHQ6WB"].size) // ligne 58 du fichier CSV 

// define JSON types for Virtual Currencies from source= Poloniex
// there are 60*60*24 = 86400 seconds in a day
// https://poloniex.com/public?command=returnChartData&currencyPair=BTC_ETH&start=1435699200&end=9999999999&period=86400
// https://poloniex.com/public?command=returnChartData&currencyPair=ETH_DAO&start=1435699200&end=9999999999&period=86400&includeDelisted=1
// dates are specified in UNIX Posix format.

type ETHBTC_f = JsonProvider<"/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/ETHBTC_Poloniex.json">
type DAOETH_f = JsonProvider<"/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/DAOETH_Poloniex.json">

let GetHistoricalData (price_table:price_table) = 

    // Real Currencies 

    // Yahoo has got only last 3 months
    //let usdjpy_yahoo = CsvFile.Load("http://ichart.finance.yahoo.com/table.csv?s=JPY=X").Cache()
    //let usdeur_yahoo = CsvFile.Load("http://ichart.finance.yahoo.com/table.csv?s=EUR=X").Cache()
    //let usdjpy = new price_timeseries()
    //[ for row in usdjpy_yahoo.Rows -> System.DateTime.Parse (row.GetColumn("Date")), float (row.GetColumn("Close"))]
    //       |> List.filter (fun (d,p)-> d>=System.DateTime.Parse("01-11-2016"))
    //       |> List.iter (fun (d,p) -> usdjpy.Add(d,p))   
    //price_table.[USD]<-usdjpy  

    // data copy-pasted from https://www.investing.com/currencies/usd-jpy-historical-data

    let usdjpy_csv = File.ReadAllLines "/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/USDJPY.csv"

    let t1' = usdjpy_csv |> Array.toList |> List.tail |> List.toArray
                         |> Array.map (fun s -> s.[0..11]) 
                         |> Array.map (System.DateTime.Parse)
                         |> Array.map (fun (d:DateTime) -> d.Date)
    let t1  = t1'        |> Array.rev

    let v1  = usdjpy_csv |> Array.toList |> List.tail |> List.toArray
                         |> Array.map (fun s -> s.[13..18]) 
                         |> Array.map (fun text -> text.Replace (";",""))
                         |> Array.map float
                         |> Array.rev

    for i in 0..(t1.Length-1) do price_table.[USD].Add (t1.[i],v1.[i])

    // add data for weekends (same than for Friday)
    // another (better ?) approach would have been to modify the get_currency_price to find the latest available price

    let start_date_usd = t1.[0]
    let end_date_usd = t1.[t1.Length - 1]
    let days_span_usd = (end_date_usd-start_date_usd).Days
    let full_dates_usd = [for i in 0..days_span_usd do yield start_date_usd.AddDays(float i)]

    full_dates_usd |> List.iter (fun d -> let d0 = Array.find (fun s-> s<=d) t1'
                                          if (d0<>d) then price_table.[USD].Add(d,price_table.[USD].[d0]) else ()
                                )

     
    let eurusd_csv = File.ReadAllLines "/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/EURUSD.csv"

    let t2' = eurusd_csv |> Array.toList |> List.tail |> List.toArray
                         |> Array.map (fun s -> s.[0..11]) 
                         |> Array.map (System.DateTime.Parse)
                         |> Array.map (fun (d:DateTime) -> d.Date)
    let t2  = t2'        |> Array.rev

    let v2 = eurusd_csv  |> Array.toList |> List.tail |> List.toArray
                         |> Array.map (fun s -> s.[13..18]) 
                         |> Array.map (fun text -> text.Replace (";",""))
                         |> Array.map float
                         |> Array.rev

    for i in 0..(t2.Length-1) do price_table.[EURUSD].Add (t2.[i],v2.[i])

    cross_2_series price_table (EURUSD,USD,EUR) multiply_float

    // add data for weekends (same than for Friday)
    // another (better ?) approach would have been to modify the get_currency_price to find the latest available price

    let start_date_eur = t2.[0]
    let end_date_eur = t2.[t2.Length - 1]
    let days_span_eur = (end_date_eur-start_date_eur).Days
    let full_dates_eur = [for i in 0..days_span_eur do yield start_date_eur.AddDays(float i)]

    full_dates_eur |> List.iter (fun d -> let d0 = Array.find (fun s-> s<=d) t2'
                                          if (d0<>d) then price_table.[EUR].Add(d,price_table.[EUR].[d0]) else ()
                                )

    printfn "so far ok"

    // Bitcoin : Source Coindesk
    // custom made parser because we have to skip the last lines

    let x = File.ReadAllLines "/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/coindesk-bpi-USD-close_data-2015-09-01_2017-02-28.csv"
    let header_size    = 1
    let signature_size = 3
    let l = x.Length

    let split (text:string)=
        text.Split [|'\t';' ';','|]

    let ParseLineIntoSeries (priceseries:price_timeseries) s =
        s |> split |> (fun res -> (System.DateTime.Parse res.[0].[1..]).Date,float res.[2])
          |> priceseries.Add

    for i in header_size..(l-signature_size-1) do
        ParseLineIntoSeries price_table.[BTCUSD] x.[i]

    cross_2_series price_table (BTCUSD,USD,BTC) multiply_float


    // Other Virtual Currencies: Source Poloniex

    // ETH

    let ethbtc = ETHBTC_f.GetSamples()

    for item in ethbtc do price_table.[ETHBTC].Add ((toDateTime item.Date).Date , float item.Close)
    cross_2_series price_table (ETHBTC,BTC,ETH) multiply_float

    // DAO 
    let daoeth = DAOETH_f.GetSamples()

    for item in daoeth do price_table.[DAOETH].Add ((toDateTime item.Date).Date , float item.Close)
    cross_2_series price_table (DAOETH,ETH,DAO) multiply_float

    for t in price_table.[DAO].Keys do
        printfn "%A %A" price_table.[DAO].[t] price_table.[ETH].[t]

let CountKeys (dict:Dictionary<'K,'T>) =
    dict.Keys |> Seq.toList |> List.length                         

[<EntryPoint>]
let main args =
   
       printfn "Arguments passed to function : %A" args

    //let stopWatch = System.Diagnostics.Stopwatch.StartNew()

       let MyT         = VirtualCoinTaxes.MyTransactions.create_empty()
       let price_table = VirtualCoinTaxes.PriceTable.init()

       // initialize historical data
       GetHistoricalData price_table

       // initialize transactions
       debug_init_myT MyT // example with 8 transactions 
       // KrakenOnlyinit MyT

       // break the transactions in pair for accounting purposes
       let D = VirtualCoinTaxes.MyTransactions.make_quark MyT price_table

       // printfn "im still a champion"

       for id1 in D.Keys do
           printfn "%A %A %A %A" (fst id1) (snd id1) D.[id1].size D.[id1].jpyprice

       // make_taxable_event1 is relevant when there is only ONE currency
       //let (id1_list1:id1 list) = [("ID0001",A);("ID0002",A);("ID0003",A);("ID0004",A)]
       //let res1 = VirtualCoinTaxes.MyTransactions.make_taxable_event1 (D) (id1_list1)

       // sort transactions by date before processing
       let id1_list2 = get_sort_id1_list_by_date D

       // First-in First-out : process the transactions to make the report 
       //let res2 = VirtualCoinTaxes.MyTransactions.make_taxable_event2 (D) (id1_list2)
       //for cur in Currency_list do printfn "%A" cur.toString
       //                            List.iter (fun (id1:id1,size) -> printfn "%A %A %A" (fst id1) (snd id1).toString size) (fst res2).[cur]

       //make_all_summary (snd res2) |> ignore

       // Average Price : process the transactions to make the report 
       let res4 = VirtualCoinTaxes.MyTransactions.make_taxable_event4 (D) (id1_list2)
       for cur in Currency_list do printfn "%A" cur.toString
                                   printfn "size = %A avg price = %A" (fst res4).[cur].size (fst res4).[cur].price
       make_all_summary (snd res4) |> ignore

       // test for checking historical prices

       let dt = (System.DateTime.Parse  "2016-01-13 16:19:47.606").Date

       printfn "USDJPY = %A EURJPY = %A" (get_currency_price (price_table) (USD,dt)) (get_currency_price (price_table) (EUR,dt))
       printfn "BTCUSD = %A BTCJPY = %A" (get_currency_price price_table (BTCUSD,dt)) (get_currency_price price_table (BTC,dt))
       // printfn "ETHBTC = %A ETHJPY = %A" (get_currency_price price_table (ETHBTC,dt)) (get_currency_price price_table (ETH,dt))

       let dtt = (System.DateTime.Parse "2016-03-13 16:19:47.606").Date

       printfn "USDJPY = %A EURJPY = %A" (get_currency_price (price_table) (USD,dtt))(get_currency_price (price_table) (EUR,dtt))
       printfn "BTCUSD = %A BTCJPY = %A" (get_currency_price price_table (BTCUSD,dtt)) (get_currency_price price_table (BTC,dtt))
       // printfn "ETHBTC = %A ETHJPY = %A" (get_currency_price price_table (ETHBTC,dt)) (get_currency_price price_table (ETH,dt))

       0
    // Return 0. This indicates success.

    //hello