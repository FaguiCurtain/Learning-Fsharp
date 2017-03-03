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


// let ICOinit (myT:transaction_DB)(price_table:price_table)=



type Poloniex = 
   CsvProvider<"/Users/francois-guillaume.rideau/Documents/crypto/trading/Poloniex tradeHistory.csv",";",
                 Schema = "Order Number = string,Fee=float">

let PoloniexOnlyinit (myT:transaction_DB)(price_table:price_table)=
    let rawfile = Poloniex.GetSample()

    let buy_or_sell (order)=
        match order with
          | "Buy"  -> 1.0
          | "Sell" -> -1.0
          | _ -> 0.0       // should not happen

    let ParsePair_Poloniex pair =
        match pair with
          | "ARDR/BTC" -> (ARDR,BTC)
          | "BTS/BTC"  -> (BTS,BTC)
          | "DAO/BTC"  -> (DAO,BTC)
          | "ETC/BTC"  -> (ETC,BTC)
          | "ETH/BTC"  -> (ETH,BTC)
          | "FCT/BTC"  -> (FCT,BTC)
          | "LTC/BTC"  -> (LTC,BTC)
          | "MAID/BTC" -> (MAID,BTC)
          | "NXT/BTC"  -> (NXT,BTC)
          | "REP/BTC"  -> (REP,BTC)
          | "SC/BTC"   -> (SC,BTC)
          | "SDC/BTC"  -> (SDC,BTC)
          | "SYS/BTC"  -> (SYS,BTC)
          | "XMR/BTC"  -> (XMR,BTC)
          | "XRP/BTC"  -> (XRP,BTC)
          | _ -> failwith "Poloniex pair not acknowledged"
    
    let keytable = new Dictionary<string,int>()
    let mutable Poloniex_fee_jpy = 0.0

    for row in rawfile.Rows do
        let txid = row.``Order Number``
        let txdate = row.Date
        let pair = ParsePair_Poloniex (row.Market)
        let (already_in,count) = keytable.TryGetValue (txid)
        // printfn "%A" row.Fee
        let fee = 0.01 * float (row.Fee) * float (row.Amount) * float (row.Price) * get_currency_price price_table (snd pair,txdate)
        // let fee = 0.0

        match already_in with
          | false -> myT.Add(txid,{time=  txdate;
                                   pair=  pair;
                                   size=  float ( buy_or_sell(row.Type)) * float (row.Amount);                                         
                                   price= float (row.Price);
                                   fee=   fee 
                                         }    )
                     keytable.Add(txid,1)
          | true -> myT.Add(txid+string count,{time=  txdate;
                                               pair=  pair;
                                               size=  float ( buy_or_sell(row.Type)) * float (row.Amount);                                         
                                               price= float (row.Price);
                                               fee= fee 
                                               }    )
                    keytable.[txid] <- count+1
        Poloniex_fee_jpy <- Poloniex_fee_jpy + fee
    printfn "Poloniex_fee_jpy = %A" Poloniex_fee_jpy
    |> ignore

let BitTrexOnly (myT:transaction_DB)(price_table:price_table) = 
    let rawfile = CsvFile.Load("/Users/francois-guillaume.rideau/Documents/crypto/trading/BitTrex trades.csv",";")

    let ParsePair_BitFinex pair =
        match pair with
          | "BTC-DGD"   -> (DGD,BTC)
          | "BTC-ARDR"  -> (ARDR,BTC)
          | _          -> failwith "BitTrex pair not acknowledged"
    
    let buy_or_sell (order)=
        match order with
          | "Limit Buy"  -> 1.0
          | "Limit Sell" -> -1.0
          | _ -> failwith "BitTrex buy/sell not acknowledged"       // should not happen
    
    let mutable BitTrex_fee_jpy = 0.0
    for row in rawfile.Rows do
        let txdate = System.DateTime.Parse (row.GetColumn("Closed Date"))
        let pair=  ParsePair_BitFinex (row.GetColumn("Market"))
        let fee    = 0.002*abs(float (row.GetColumn("Units Total")))*float (row.GetColumn("Bid/Ask")) * get_currency_price price_table (snd pair,txdate)
        myT.Add(row.GetColumn("txid"),{time=  txdate;
                                       pair=  pair;
                                       size=  float ( buy_or_sell(row.GetColumn("Type"))) * float (row.GetColumn("Units Total"));                                         
                                       price= float (row.GetColumn("Bid/Ask"));
                                       fee=   fee
                                       }    )
        BitTrex_fee_jpy <- BitTrex_fee_jpy + fee
    printfn "BitTrex_fee (jpy) = %A" BitTrex_fee_jpy

let BitFinexOnly (myT:transaction_DB)(price_table:price_table) = 
    let rawfile = CsvFile.Load("/Users/francois-guillaume.rideau/Documents/crypto/trading/BitFinex tradess.csv",";")

    let ParsePair_BitFinex pair =
        match pair with
          | "BTCUSD"   -> (BTC,USD)
          | "ETHUSD"   -> (ETH,USD)
          | "ETHBTC"   -> (ETH,BTC)
          | _          -> failwith "BitFinex pair not acknowledged"
    
    let mutable BitFinex_fee_jpy = 0.0
    for row in rawfile.Rows do
        let txdate = System.DateTime.Parse (row.GetColumn("Date"))
        let pair   = ParsePair_BitFinex (row.GetColumn("Pair"))
        let fee    = 0.002*abs(float (row.GetColumn("Amount")))*float (row.GetColumn("Price")) * get_currency_price price_table (snd pair,txdate)
        myT.Add(row.GetColumn("txid"),{time=  txdate;
                                       pair=  pair;
                                       size=  float (row.GetColumn("Amount"));                                         
                                       price= float (row.GetColumn("Price"));
                                       fee=  fee 
                                       }    )
        BitFinex_fee_jpy <- BitFinex_fee_jpy + fee
        printfn "%A %A" (row.GetColumn("txid")) fee
    printfn "Bitfinex fee (jpy) total = %A" BitFinex_fee_jpy

let KrakenOnlyinit (myT:transaction_DB)(price_table:price_table) =

    let rawfile = CsvFile.Load("/Users/francois-guillaume.rideau/Documents/crypto/trading/Kraken trades.csv")

    let buy_or_sell (order)=
        match order with
          | "buy"  -> 1.0
          | "sell" -> -1.0
          | _ -> 0.0       // should not happen
    
    let ParsePair_Kraken pair =
        match pair with
          | "XDAOXETH" -> (DAO,ETH)
          | "XETCXXBT" -> (ETC,BTC)
          | "XETCZEUR" -> (ETC,EUR)
          | "XETHXXBT" -> (ETH,BTC)
          | "XETHZEUR" -> (ETH,EUR)
          | "XXBTZEUR" -> (BTC,EUR)
          | "XXRPXXBT" -> (XRP,BTC)
          | _ -> failwith "Kraken pair not acknowledged"

    let mutable kraken_fee_jpy = 0.0
    
    for row in rawfile.Rows do
        let txdate = System.DateTime.Parse (row.GetColumn("time"))
        let fee = float (row.GetColumn("fee")) * get_currency_price price_table (EUR,txdate)
        myT.Add(row.GetColumn("txid"),{time=  txdate;
                                       pair=  ParsePair_Kraken (row.GetColumn("pair"));
                                       size=  float ( buy_or_sell(row.GetColumn("type")) * float (row.GetColumn("vol")));                                         
                                       price= float (row.GetColumn("price"));
                                       fee=   fee 
                                       }    )
        kraken_fee_jpy <- kraken_fee_jpy + fee

    printfn "kraken_fee = %A" kraken_fee_jpy
    |> ignore
    // printfn "%f" (myT.["TRSYD5-F2NKZ-KHQ6WB"].size) // ligne 58 du fichier CSV 

// define JSON types for Virtual Currencies from source= Poloniex
// there are 60*60*24 = 86400 seconds in a day
// https://poloniex.com/public?command=returnChartData&currencyPair=BTC_ETH&start=1435699200&end=9999999999&period=86400

// https://poloniex.com/public?command=returnChartData&currencyPair=BTC_ARDR&start=1435699200&end=9999999999&period=86400
// https://poloniex.com/public?command=returnChartData&currencyPair=ETH_DAO&start=1435699200&end=9999999999&period=86400&includeDelisted=1
// dates are specified in UNIX Posix format.

type ETHBTC_f  = JsonProvider<"/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/ETHBTC_Poloniex.json">

type ARDRBTC_f = JsonProvider<"/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/ARDRBTC_Poloniex.json">
type BTSBTC_f  = JsonProvider<"/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/BTSBTC_Poloniex.json">
type ETCBTC_f  = JsonProvider<"/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/ETCBTC_Poloniex.json">
type FCTBTC_f  = JsonProvider<"/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/FCTBTC_Poloniex.json">
type LTCBTC_f  = JsonProvider<"/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/LTCBTC_Poloniex.json">
type MAIDBTC_f = JsonProvider<"/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/MAIDBTC_Poloniex.json">
type NXTBTC_f  = JsonProvider<"/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/NXTBTC_Poloniex.json">
type REPBTC_f  = JsonProvider<"/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/REPBTC_Poloniex.json">
type SCBTC_f   = JsonProvider<"/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/SCBTC_Poloniex.json">
type SDCBTC_f  = JsonProvider<"/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/SDCBTC_Poloniex.json">
type SYSBTC_f  = JsonProvider<"/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/SYSBTC_Poloniex.json">
type XMRBTC_f  = JsonProvider<"/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/XMRBTC_Poloniex.json">
type XRPBTC_f  = JsonProvider<"/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/XRPBTC_Poloniex.json">

type DAOETH_f  = JsonProvider<"/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/DAOETH_Poloniex.json">

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

    // DAO (special case as quoted vs ETH)
    let daoeth = DAOETH_f.GetSamples()

    for item in daoeth do price_table.[DAOETH].Add ((toDateTime item.Date).Date , float item.Close)
    cross_2_series price_table (DAOETH,ETH,DAO) multiply_float

    for t in price_table.[DAO].Keys do
        printfn "%A %A" price_table.[DAO].[t] price_table.[ETH].[t]
    
    //let get_Poloniex_data (data:JsonProvider)(cur1,cur2) = // doesn't work
    //    let samples = data.GetSamples()
    //    for item in samples do price_table.[cur1].Add ((toDateTime item.Date).Date , float item.Close)
    //    cross_2_series price_table (cur1,BTC,cur2) multiply_float

    //  ARDR
    let ardrbtc = ARDRBTC_f.GetSamples()

    for item in ardrbtc do price_table.[ARDRBTC].Add ((toDateTime item.Date).Date , float item.Close)
    cross_2_series price_table (ARDRBTC,BTC,ARDR) multiply_float

    //  BTS
    let btsbtc  = BTSBTC_f.GetSamples()

    for item in btsbtc do price_table.[BTSBTC].Add ((toDateTime item.Date).Date , float item.Close)
    cross_2_series price_table (BTSBTC,BTC,BTS) multiply_float

    //  ETC
    let etcbtc  = ETCBTC_f.GetSamples()

    for item in etcbtc do price_table.[ETCBTC].Add ((toDateTime item.Date).Date , float item.Close)
    cross_2_series price_table (ETCBTC,BTC,ETC) multiply_float

    //  FCT
    let fctbtc  = FCTBTC_f.GetSamples()

    for item in fctbtc do price_table.[FCTBTC].Add ((toDateTime item.Date).Date , float item.Close)
    cross_2_series price_table (FCTBTC,BTC,FCT) multiply_float

    //  LTC
    let ltcbtc  = LTCBTC_f.GetSamples()

    for item in ltcbtc do price_table.[LTCBTC].Add ((toDateTime item.Date).Date , float item.Close)
    cross_2_series price_table (LTCBTC,BTC,LTC) multiply_float

    //  MAID
    let maidbtc = MAIDBTC_f.GetSamples()

    for item in maidbtc do price_table.[MAIDBTC].Add ((toDateTime item.Date).Date , float item.Close)
    cross_2_series price_table (MAIDBTC,BTC,MAID) multiply_float

    //  NXT
    let nxtbtc  = NXTBTC_f.GetSamples()

    for item in nxtbtc do price_table.[NXTBTC].Add ((toDateTime item.Date).Date , float item.Close)
    cross_2_series price_table (NXTBTC,BTC,NXT) multiply_float

    //  REP
    let repbtc  = REPBTC_f.GetSamples()

    for item in repbtc do price_table.[REPBTC].Add ((toDateTime item.Date).Date , float item.Close)
    cross_2_series price_table (REPBTC,BTC,REP) multiply_float

    //  SC
    let scbtc  = SCBTC_f.GetSamples()

    for item in scbtc do price_table.[SCBTC].Add ((toDateTime item.Date).Date , float item.Close)
    cross_2_series price_table (SCBTC,BTC,SC) multiply_float

    //  SDC
    let sdcbtc  = SDCBTC_f.GetSamples()

    for item in sdcbtc do price_table.[SDCBTC].Add ((toDateTime item.Date).Date , float item.Close)
    cross_2_series price_table (SDCBTC,BTC,SDC) multiply_float

    //  SYS
    let sysbtc  = SYSBTC_f.GetSamples()

    for item in sysbtc do price_table.[SYSBTC].Add ((toDateTime item.Date).Date , float item.Close)
    cross_2_series price_table (SYSBTC,BTC,SYS) multiply_float

    //  XMR
    let xmrbtc  = XMRBTC_f.GetSamples()

    for item in xmrbtc do price_table.[XMRBTC].Add ((toDateTime item.Date).Date , float item.Close)
    cross_2_series price_table (XMRBTC,BTC,XMR) multiply_float

    // XRP
    let xrpbtc  = XRPBTC_f.GetSamples()

    for item in xrpbtc do price_table.[XRPBTC].Add ((toDateTime item.Date).Date , float item.Close)
    cross_2_series price_table (XRPBTC,BTC,XRP) multiply_float

    // end of GetHistoricalData //


let CountKeys (dict:Dictionary<'K,'T>) =
    dict.Keys |> Seq.toList |> List.length                         


[<EntryPoint>]
let main args =
   
       printfn "Arguments passed to function : %A" args

    //let stopWatch = System.Diagnostics.Stopwatch.StartNew()

       let MyT         = VirtualCoinTaxes.MyTransactions.create_empty()
       let price_table = VirtualCoinTaxes.PriceTable.init()

       // initialize historical data. need to do that before initializing transactions to calculate fees in JPY and break transactions
       GetHistoricalData price_table

       // initialize transactions
       // debug_init_myT MyT // example with 8 transactions 
       KrakenOnlyinit MyT price_table
       PoloniexOnlyinit MyT price_table
       BitFinexOnly MyT price_table
       BitTrexOnly MyT price_table

       // break the transactions in pair for accounting purposes
       let D = VirtualCoinTaxes.MyTransactions.make_quark MyT price_table

       // printfn "im still a champion"

       // print to console all the transactions
       //for id1 in D.Keys do
       //     printfn "%A %A %A %A" (fst id1) (snd id1) D.[id1].size D.[id1].jpyprice

       // make_taxable_event1 is relevant when there is only ONE currency
       //let (id1_list1:id1 list) = [("ID0001",A);("ID0002",A);("ID0003",A);("ID0004",A)]
       //let res1 = VirtualCoinTaxes.MyTransactions.make_taxable_event1 (D) (id1_list1)

       // sort transactions by date before processing
       let id1_list2 = get_sort_id1_list_by_date D

       // First-in First-out : process the transactions to make the report 
       let res2 = VirtualCoinTaxes.MyTransactions.make_taxable_event2 (D) (id1_list2)
       for cur in Currency_list do printfn "%A" cur.toString
                                   List.iter (fun (id1:id1,size) -> printfn "%A %A %A" (fst id1) (snd id1).toString size) (fst res2).[cur]

       make_all_summary (snd res2) |> ignore

       // Average Price : process the transactions to make the report 
       let res4 = VirtualCoinTaxes.MyTransactions.make_taxable_event4 (D) (id1_list2)
       for cur in Currency_list do let size = (fst res4).[cur].size
                                   if size <> 0.0 then 
                                                    printfn "%A" cur.toString
                                                    printfn "size = %A avg price = %A" size (fst res4).[cur].price
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