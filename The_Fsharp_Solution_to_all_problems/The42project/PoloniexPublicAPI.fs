open System
open System.IO
open System.Net

open FSharp.Data
open XPlot.GoogleCharts

open Deedle

type Filter<'T> = Filter of ('T -> bool) with 
     static member (|*) (Filter f,Filter g) = Filter (fun x -> f x || g x)       // OR
     static member (&*) (Filter f,Filter g) = Filter (fun x -> (f x) && (g x))   // AND
     static member (!*) (Filter f)          = Filter (fun x -> not (f x))        // NOT
     static member (<>*) (Filter f,Filter g) = Filter (fun x -> (f x) <> (g x))  // XOR
     member this.unwrap = let (Filter f) = this 
                          f // needs to take unit
/// map2series
let map2series (f:'T1->'T2->'R)(series1:Series<'K,'T1 opt>)(series2:Series<'K,'T2 opt>):Series<'K,'R opt>=
     let S = series1.Zip(series2,JoinKind.Outer) //Series<'K,('T1 opt opt * 'T2 opt opt)>

     S |> Series.mapValues (fun (a,b) -> match (a,b) with 
                                          | (OptionalValue.Present(a'), OptionalValue.Present(b')) -> OptionalValue.map2 f a' b'
                                          | _ -> OptionalValue.Missing)

let some f =
    function
      | (OptionalValue.Present(a'), OptionalValue.Present(b')) -> Some (f a' b')
      | _ -> None

let map2series2 f series1 =
     Series.zip series1
       >> Series.mapAll(fun k -> Option.bind(some f))                          
 
/// converts a Series<'K,'T option> back to Series<'K,'T>
let convert series = 
    series |> Series.mapAll (fun _ v -> v |> Option.bind id)
/// converts a Series<'K,'T opt> back to Series<'K,'T>
let convert1 series = 
    series |> Series.mapAll (fun _ v -> v |> Option.bind OptionalValue.asOption)

/// sum of (series + series.Shift 1 + series.Shift2 + ... + series.Shift n)
// recursive implementation
//let slidingsum_f (n:int) (series:Series<'K,float>) = 
    //let rec helper k acc =
    //    if k <0 then failwith "error: negative argument"
    //            elif k=0 then acc
    //            else helper (k-1) (acc + series.Shift k)
    //helper n series

let slidingsum_f (n:int) (series:Series<'K,float>) = 
    series |> Series.windowInto n (fun S -> S.NumSum()) 

type Size = float
type EntryDate = DateTime
type EntryLvl = float
type StopLvl = float
type ExitLvl = float

type Position = 
     | Pos of Size * EntryDate * EntryLvl * StopLvl
     | NoPos

/// Aggregates a series of format Open
type Low = Low of float
type High = High of float
type Open = Open of float
type Close = Close of float
type Volume = Volume of float
type WAP = WAP of float
type Candle = (Low * Open * Close * High * Volume * WAP)

// these functions return float type

let getCandleLow (candle:Candle) =
    let (Low l,_,_,_,_,_) = candle
    l

let getCandleHigh (candle:Candle) =
    let _,_,_,High h,_,_ = candle
    h

let getCandleOpen (candle:Candle) =
    let _,Open o,_,_,_,_ = candle
    o

let getCandleClose (candle:Candle) =
    let _,_,Close c,_,_,_ = candle
    c

let getCandleVolume (candle:Candle) =
    let _,_,_,_,Volume v,_ = candle
    v

let getCandleWAP (candle:Candle) = 
    let _,_,_,_,_,WAP w = candle
    w


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

let GetPairHistory (pair:string) =
    let commandType1 = "returnChartData"
    let currencyPair1 = pair // for example "BTC_ETH"
    let start_date1 = string 1405699200M // already defined up there
    let end_date1   = string 9999999999M
    let period1     = string 300M
    let period1b    = string 14400M

    let polReq = "https://poloniex.com/public?" + "command=" + commandType1 + "&currencyPair=" + currencyPair1 + "&start=" + start_date1
                  + "&end=" + end_date1 + "&period=" + period1
    CallAPI_and_save_JSON (polReq,"/Users/francois-guillaume.rideau/Documents/crypto/trading/backtesting/"+pair+".json")

let Currency_list = ["BTC_XRP";"BTC_ZEC"]

let jsonFilename = "/Users/francois-guillaume.rideau/Documents/chart_data.json"
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
let currencyPair1 = "BTC_ETH"
let start_date1 = string 1405699200M // already defined up there
let end_date1   = string 9999999999M
let period1     = string 300M
let period1b    = string 14400M

let polReq1 = "https://poloniex.com/public?" + "command=" + commandType1 + "&currencyPair=" + currencyPair1 + "&start=" + start_date1
              + "&end=" + end_date1 + "&period=" + period1

let polReq1b = "https://poloniex.com/public?" + "command=" + commandType1 + "&currencyPair=" + currencyPair1 + "&start=" + start_date1
              + "&end=" + end_date1 + "&period=" + period1b

    




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
    CallAPI_and_save_JSON (polReq1b,"/Users/francois-guillaume.rideau/Documents/chart_data.json")

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

    //////////////////////////////////////////////////////////////////////////
    // ACHTUNG !!!!!! THIS DOESN'T WORK INSIDE VS MAC AT THE MOMENT !!!!!!! //
    //////////////////////////////////////////////////////////////////////////
    let ChartNow = ChartData.Load(polReq1)  // 5mn
    let ChartNow = ChartData.Load(polReq1b) //daily


    // too much data to visualize
    // [for x in ChartNow do yield (x.Date,x.Low,x.Open,x.Close,x.High)]

    // date_b and date_e are included
    let l = ChartNow.Length
    let b = ChartNow.[24].Date  |> toDateTime
    let e = ChartNow.[l-1].Date |> toDateTime
    let date_b = DateTime(b.Year,b.Month,b.Day,b.Hour+1,0,0)   
    let date_e = DateTime(e.Year,e.Month,e.Day,e.Hour-1,55,0)  

    //let ch = [for i in (l-19)..(l-1)   do let x = ChartNow.[i]
    //                                      // yield (x.Date,x.Low,x.Open,x.Close,x.High)
    //                                      yield ((toDateTime x.Date),x.Low,x.Open,x.Close,x.High)
    //                      ]
    //         |> Chart.Candlestick 
    //ch.Show()

    let s= Series.ofObservations( seq {for i in (l-199)..(l-1)   do 
                                                                    let x = ChartNow.[i]
                                                                    yield ((toDateTime x.Date),float x.Close)
                            }  )

    let s= Series.ofObservations( seq {for i in 0..(l-1)   do 
                                                                    let x = ChartNow.[i]
                                                                    yield ((toDateTime x.Date),float x.Close)
                            }  )   

    


    let sr = (Series.diff 1 s)/s // series of returns
    Stats.kurt sr
    Stats.skew sr

    let (mychart:Series<DateTime,Candle>) = Series.ofObservations( seq {for i in 24..(l-1)   do 
                                                                        let x = ChartNow.[i]
                                                                        let d = toDateTime x.Date
                                                                        if (d<=date_e) && (d>=date_b) then 
                                                                           yield ((toDateTime x.Date),(Low (float x.Low),Open (float x.Open),Close (float x.Close),High (float x.High),
                                                                                                       Volume (float x.Volume),WAP (float x.WeightedAverage) ))
                                                                         }  )
    /// gets the low of a series of candle
    let getlow (s:Series<DateTime,Candle>) =

        let a = s |> Series.map (fun d c -> getCandleLow c) |> Stats.min
        match a with 
           | None -> failwith "Error: while calculating low of Series"
           | Some l -> Low l
    /// gets the high of a series of candle
    let gethigh (s:Series<DateTime,Candle>) =

        let a = s |> Series.map (fun d c -> getCandleHigh c) |> Stats.max
        match a with 
           | None -> failwith "Error: while calculating max of Series"
           | Some l -> High l
    /// gets the open of a series of candle, i.e. the first open
    let getopen (s:Series<DateTime,Candle>) = 
        let c = Series.firstValue s
        let _,o,_,_,_,_ = c
        o
    /// gets the close of a series of candle, i.e. the last close
    let getclose (s:Series<DateTime,Candle>) = 
        let c = Series.lastValue s
        let _,_,cl,_,_,_= c
        cl
    ///gets the volume of a series of candle, i.e. sums the volumes, and the WAP
    let getvolume_and_wap(s:Series<DateTime,Candle>) = 
        let v  = s |> Series.map (fun d c -> getCandleVolume c) |> Stats.sum
        let w' = s |> Series.map (fun d c -> (getCandleVolume c)*(getCandleWAP c)) |> Stats.sum
        (Volume v,WAP (w'/v))




    let unwrap_candle_series (s:Series<DateTime,Candle>) = 
        s |> Series.map (fun d c -> let (Low l,Open o,Close cl, High h,Volume v, WAP w) = c
                                    (l,o,cl,h,v,w) )

    // in our example, resampling a 5mn chart into a 60mn chart,
    // with Direction.Forward, the Open of the 3pm hourly candle is the open of the 3pm candle, and the Close of the 3pm hourly candle is the close of the 3:55pm candle

    let (hourlychart:Series<DateTime,Candle>) =  mychart |> Series.sampleTimeInto (TimeSpan (1,0,0)) Direction.Forward (fun s -> let (v,w) = getvolume_and_wap s
                                                                                                                                 getlow s,getopen s,getclose s,gethigh s,v,w)

                                                                                       
                               
    
    let s' = hourlychart |> Series.map (fun d c -> getCandleClose c)
    let sr' = (Series.diff 1 s')/s' // series of PAST returns
    Stats.kurt sr'
    Stats.skew sr'


                                                                         
    /// gets the (hourly) volume average in a sliding window of size 24
    let w_size_vol = 24
    let OrigVolume = hourlychart |> Series.mapValues getCandleVolume
    let AvgVolume = OrigVolume |> Series.windowInto w_size_vol Stats.mean |> Series.shift +1
    let StdVolume = OrigVolume |> Series.windowInto w_size_vol Stats.stdDev |> Series.shift +1

    // df.GetRowAt<float> 30
    // df |> Frame.filterRows (fun _ row -> row?OrigVolume > 100.0)
    // df |> Frame.filterRows (fun _ row -> row?OrigVolume > row?AvgVolume + 2.0*row?StdVolume)

    let w_size_close = 10
    let OrigClose = hourlychart |> Series.mapValues getCandleClose
    let AvgClose  = OrigClose |> Series.windowInto w_size_close Stats.mean |> Series.shift 0    // it's ok to use 0 ONLY because we trade at next Open.
    let StdClose  = OrigClose |> Series.windowInto w_size_close Stats.stdDev |> Series.shift 0
    let MaxClose  = OrigClose |> Series.windowInto w_size_close Stats.max 
                              |> Series.mapValues (fun o -> match o with // o pour option
                                                             | Some x -> x
                                                             | None -> failwith "Error when calculating MaxClose of series")
                              |> Series.shift +1

    let MinClose = OrigClose  |> Series.windowInto w_size_close Stats.min
                              |> Series.mapValues (fun o -> match o with 
                                                             | Some x -> x
                                                             | None -> failwith "Error when calculating MinClose of series")
                              |> Series.shift +1

    // defined on a 5 minute chart
    let Close = mychart |> Series.mapValues getCandleClose  
    let HourlyAvgClose = mychart |> Series.map (fun d c -> let d' = DateTime(d.Year,d.Month,d.Day,d.Hour,0,0)-TimeSpan(1,0,0)
                                                           
                                                           Series.tryGet d' AvgClose)
                                 |> convert



    let HourlyStdClose = mychart |> Series.map (fun d c -> let d' = DateTime(d.Year,d.Month,d.Day,d.Hour,0,0)-TimeSpan(1,0,0)
                                                           
                                                           Series.tryGet d' StdClose)
                                 |> convert 
 
    let HourlyMaxClose = mychart |> Series.map (fun d c -> let d' = DateTime(d.Year,d.Month,d.Day,d.Hour,0,0)-TimeSpan(1,0,0)
                                                                                        
                                                           Series.tryGet d' MaxClose)
                                 |> convert                             

    let Volume = mychart |> Series.mapValues getCandleVolume
                         |> slidingsum_f 11 // ce n'est pas ce qu'on veut

    let HourlyAvgVolume = mychart |> Series.map (fun d c -> let d' = DateTime(d.Year,d.Month,d.Day,d.Hour,0,0)-TimeSpan(1,0,0)
                                                           
                                                            Series.tryGet d' AvgVolume)
                                  |> convert

    let HourlyStdVolume = mychart |> Series.map (fun d c -> let d' = DateTime(d.Year,d.Month,d.Day,d.Hour,0,0)-TimeSpan(1,0,0)
                                                           
                                                            Series.tryGet d' StdVolume)
                                  |> convert
    



    // RSI Indicator

    let RSI_n = 14

    // HOURLY

    let Hourly_U = (Series.diff 1 OrigClose) |> Series.mapValues (fun x -> max x 0.0)
    let Hourly_D = (Series.diff 1 OrigClose) |> Series.mapValues (fun x -> min x 0.0)
    let Hourly_Mean14_U = Hourly_U |> Series.windowInto RSI_n Stats.mean 
    let Hourly_Mean14_D = -Hourly_D |> Series.windowInto RSI_n Stats.mean
    let HourlyRSI = 100.0 * HourlyMean14_U / (HourlyMean14_U + HourlyMean14_D)

    // 5MNS

    let _5mn_U = (Series.diff 1 Close) |> Series.mapValues (fun x -> max x 0.0)
    let _5mn_D = (Series.diff 1 Close) |> Series.mapValues (fun x -> min x 0.0)
    let _5mn_Mean14_U =   _5mn_U |> Series.windowInto RSI_n Stats.mean 
    let _5mn_Mean14_D = - _5mn_D |> Series.windowInto RSI_n Stats.mean
    let _5mn_RSI = 100.0 * _5mn_Mean14_U / (_5mn_Mean14_U + _5mn_Mean14_D)


    // defined on the 5mn 
    let lastHourlyRSI = mychart  |> Series.map (fun d c -> let d' = DateTime(d.Year,d.Month,d.Day,d.Hour,0,0)-TimeSpan(1,0,0)
                                                           
                                                           Series.tryGet d' HourlyRSI)
                                  |> convert

    // The Exec_Price is the Open of the next 5mn candle        
    let exec_price_1 = (mychart |> Series.mapValues getCandleOpen).Shift -1 
     

    let df = Frame.ofColumns["OrigVolume"=>OrigVolume;"AvgVolume"=>AvgVolume;"StdVolume"=>StdVolume;
                             "OrigClose"=>OrigClose;"AvgClose"=>AvgClose;"StdClose"=>StdClose;"MaxClose"=>MaxClose;"MinClose"=>MinClose]
    // let df1 = Frame.ofColumns["OrigClose"=>OrigClose;"AvgClose"=>AvgClose;"StdClose"=>StdClose;"MaxClose"=>MaxClose];;

    // this approach is not vectorized so possibly slower ?
    let df1 = df  |> Frame.filterRows (fun _ row -> ((row?OrigClose > row?AvgClose + 1.0*row?StdClose) ||
                                                     (row?OrigClose > row?MaxClose)) &&
                                                     (row?OrigVolume > row?AvgVolume + 2.0*row?StdVolume))
   
    let signal_dates_bull = df1.RowKeys


    let df2 = df  |> Frame.filterRows (fun _ row -> ((row?OrigClose > row?AvgClose - 1.0*row?StdClose) ||
                                                     (row?OrigClose < row?MinClose)) &&
                                                     (row?OrigVolume > row?AvgVolume + 2.0*row?StdVolume))
   
    let signal_dates_bear = df2.RowKeys

    let dff = Frame.ofColumns["Volume"=?>Volume;"HourlyAvgVolume"=?>HourlyAvgVolume;"HourlyStdVolume"=?>HourlyStdVolume;
                              "Close"=?>Close;"HourlyAvgClose"=?>HourlyAvgClose;"HourlyStdClose"=?>HourlyStdClose;"HourlyMaxClose"=?>HourlyMaxClose;
                              "lastHourlyRSI"=?>lastHourlyRSI;
                              "Exec_Price"=?>exec_price_1]

    // dff.GetColumn<float opt>("AvgHourlyClose")
    (Close - (HourlyAvgClose + HourlyStd)) |> Series.filterValues (fun x-> x>0.0)


    let df1' = dff |> Frame.filterRows (fun _ row -> ((row?Close  > row?HourlyAvgClose +  1.0*row?HourlyStdClose) ||
                                                      (row?Close  > row?HourlyMaxClose)) &&
                                                      (row?Volume > row?HourlyAvgVolume + 1.6*row?HourlyStdVolume))
    // adding RSI condition
    let df1' = dff |> Frame.filterRows (fun _ row -> ((row?Close  > row?HourlyAvgClose +  1.0*row?HourlyStdClose) ||
                                                      (row?Close  > row?HourlyMaxClose)) &&
                                                      (row?Volume > row?HourlyAvgVolume + 1.6*row?HourlyStdVolume) &&
                                                      (row?lastHourlyRSI)<=70.0
                                                      )
    
    let signal_dates_bull_1 = df1'.RowKeys

    // MAKE A STRATEGY
    let exec_price = df.GetColumn<float>("OrigClose") // executable price = hourly close


    // BREAKOUT strategy based on hourly chart version 1


    let bullish_breakout_strat1 (start_date:DateTime) (end_date:DateTime) =

        let time_increment = TimeSpan(0,0,5)
        let holding_period = TimeSpan(6,0,0)

        let make_trade (trade:Series<DateTime,bool*EntryLvl>) (tradedate,tradelevel) = // not optimized at all !!!!!!
            Series.map (fun k v -> if (k=tradedate) then (true,tradelevel) else v) trade


        let rec helper (date:DateTime) (pos:Position) (trade:Series<DateTime,bool*EntryLvl>) =
            printfn "%A" date
            if date > end_date then (pos,trade) else
               match pos with
                 | Pos (size,entrydate,entrylvl,stoplvl) -> 
                            if date < (entrydate + holding_period) then 
                                // implement later : hold unless stoploss 
                                 helper (date+time_increment) (pos) (trade)
                            else helper (date+time_increment) (NoPos) (trade) // close trade
                 | NoPos   -> if Seq.exists (fun d->d=date) signal_dates_bull_1// if there is a signal
                                 then helper (date+time_increment) (Pos (1.0,date,0.0,0.0)) (make_trade (trade) (date,exec_price_1.[date])) // make a trade
                              else helper (date+time_increment) (NoPos) (trade) // else rebalance or do nothing

        let init_trade = dff.RowKeys |> Seq.map (fun d-> (d,(false,-1.0))) |> Series.ofObservations
        helper start_date NoPos init_trade
    // TOO SLOW implementation
    // let trade_dates_bull1 = snd (bullish_breakout_strat1 start_date end_date) |> Series.filter (fun k v -> fst v=true)




    let (start_date,end_date) = mychart.KeyRange

    // Refactor without recursive functions with Seq.Fold
    // make sure there is no missing data and keys

    let folder (pos:Position,trade:(DateTime*EntryLvl) list) (current_date:DateTime) = 

        let make_trade (trade:(DateTime*EntryLvl) list) (tradedate,tradelevel) = // not optimized at all !!!!!!
            (tradedate,tradelevel)::trade

        // printfn "%A" current_date
        let holding_period = TimeSpan(6,0,0)
        match pos with
          | Pos (size,entrydate,entrylvl,stoplvl) -> 
                     if current_date < (entrydate + holding_period) then 
                         // implement later : hold unless stoploss 
                        (pos,trade)
                     else (NoPos,trade) // close trade
          | NoPos   -> if Seq.exists (fun d->d=current_date) signal_dates_bull_1// if there is a signal
                          then (Pos (1.0,current_date,0.0,0.0) , (make_trade (trade) (current_date,exec_price_1.[current_date])) )// make a trade
                       else  (NoPos,trade) // else rebalance or do nothing

    let res = Seq.fold folder (NoPos,[]) mychart.Keys
    let trade_dates_bull = (snd res) |> List.map fst

    // IMPLEMENT WITH STOP LOSSES

type LowSoFar = float
type HighSoFar = float
type NoTradeTill = DateTime

type Position1 = 
     | Pos1 of Size * EntryDate * EntryLvl * StopLvl * LowSoFar * HighSoFar
     | NoPos1

type TradeHorizon =
     | DateLimit of DateTime
     | NoHorizon


    let folder1 (pos:Position1,horizon:TradeHorizon,trade:(Size*DateTime*EntryLvl) list) (current_date:DateTime) = 

        let make_trade (trade:(Size*DateTime*EntryLvl) list) (size,tradedate,tradelevel) = 
            (size,tradedate,tradelevel)::trade

        // printfn "%A" current_date
        let holding_period = TimeSpan(6,0,0)

        match pos with
          | Pos1 (size,entrydate,entrylvl,stoplvl,lowsofar,highsofar) -> 
                     match horizon with 
                       | DateLimit datelimit -> if current_date < datelimit
                                                       then if ( (mychart.[current_date] |> getCandleLow) <= stoplvl )// if stop-loss hit
                                                               then let trade'= make_trade trade (-1.0,current_date,stoplvl) // close trade at stoplvl
                                                                    (NoPos1,horizon,trade')                                                               
                                                            else let m = min lowsofar  (mychart.[current_date] |> getCandleLow )
                                                                 let M = max highsofar (mychart.[current_date] |> getCandleHigh)
                                                                 let pos' = Pos1(size,entrydate,entrylvl,stoplvl,m,M)
                                                                 (pos',horizon,trade)     //hold
                                                 
                                                else let trade'= make_trade trade (-1.0,current_date,exec_price_1.[current_date])// close trade because of time limit
                                                     (NoPos1,NoHorizon,trade')
                       | NoHorizon -> if ( (mychart.[current_date] |> getCandleLow) <= stoplvl )// if stop-loss hit
                                         then let trade'= make_trade trade (-1.0,current_date,stoplvl) // close trade at stoplvl
                                              (NoPos1,horizon,trade')       
                                      else let m = min lowsofar  (mychart.[current_date] |> getCandleLow )
                                           let M = max highsofar (mychart.[current_date] |> getCandleHigh)
                                           let pos' = Pos1(size,entrydate,entrylvl,stoplvl,m,M)
                                                                                         
                                           (pos',horizon,trade)     //hold
                                      
          | NoPos1   -> match horizon with 
                         | NoHorizon ->
                             if Seq.exists (fun d->d=current_date) signal_dates_bull_1// if there is a signal
                                then let (entrylvl,stoplvl)= (exec_price_1.[current_date],exec_price_1.[current_date]*0.97)
                                     let trade' = make_trade trade (1.0,current_date,entrylvl) // make a trade
                                     // we forget the case where we are immediately stopped out for the time being ?
                                     let pos' = Pos1(1.0,current_date,entrylvl,stoplvl,entrylvl,entrylvl)
                                     (pos',DateLimit (current_date+holding_period),trade')
                             else  (NoPos1,NoHorizon,trade) // else rebalance or do nothing
                         | DateLimit datelimit ->
                             if current_date < datelimit 
                                then (pos,horizon,trade)
                             else (pos,NoHorizon,trade) // limit expires


    /// the trade list includes the Exit level in this view

    let folder1A (pos:Position1,horizon:TradeHorizon,trade:(Size*DateTime*EntryLvl*ExitLvl) list) (current_date:DateTime) = 

        let make_trade  (trade:(Size*DateTime*EntryLvl*ExitLvl) list) (size,tradedate,tradelevel) = 
            (size,tradedate,tradelevel,tradelevel)::trade

        let close_trade (trade:(Size*DateTime*EntryLvl*ExitLvl) list) (tradelevel) = // doesn't take size as argument nor the exit trade date to simplify
            let (s,d,en,_) = trade.Head
            (s,d,en,tradelevel)::trade.Tail

        // printfn "%A" current_date
        let holding_period = TimeSpan(6,0,0)

        match pos with
          | Pos1 (size,entrydate,entrylvl,stoplvl,lowsofar,highsofar) -> 
                     match horizon with 
                       | DateLimit datelimit -> if current_date < datelimit
                                                       then if ( (mychart.[current_date] |> getCandleLow) <= stoplvl )// if stop-loss hit
                                                               then let trade'= close_trade trade (stoplvl) // close trade at stoplvl
                                                                    (NoPos1,horizon,trade')                                                               
                                                            else let m = min lowsofar  (mychart.[current_date] |> getCandleLow )
                                                                 let M = max highsofar (mychart.[current_date] |> getCandleHigh)
                                                                 let pos' = Pos1(size,entrydate,entrylvl,stoplvl,m,M)
                                                                 (pos',horizon,trade)     //hold
                                                 
                                                else let trade'= close_trade trade (exec_price_1.[current_date])// close trade because of time limit
                                                     (NoPos1,NoHorizon,trade')
                       | NoHorizon -> if ( (mychart.[current_date] |> getCandleLow) <= stoplvl )// if stop-loss hit
                                         then let trade'= close_trade trade (stoplvl) // close trade at stoplvl
                                              (NoPos1,horizon,trade')       
                                      else let m = min lowsofar  (mychart.[current_date] |> getCandleLow )
                                           let M = max highsofar (mychart.[current_date] |> getCandleHigh)
                                           let pos' = Pos1(size,entrydate,entrylvl,stoplvl,m,M)
                                                                                         
                                           (pos',horizon,trade)     //hold
                                      
          | NoPos1   -> match horizon with 
                         | NoHorizon ->
                             if Seq.exists (fun d->d=current_date) signal_dates_bull_1// if there is a signal
                                then let (entrylvl,stoplvl)= (exec_price_1.[current_date],exec_price_1.[current_date]*0.97)
                                     let trade' = make_trade trade (1.0,current_date,entrylvl) // make a trade
                                     // we forget the case where we are immediately stopped out for the time being ?
                                     let pos' = Pos1(1.0,current_date,entrylvl,stoplvl,entrylvl,entrylvl)
                                     (pos',DateLimit (current_date+holding_period),trade')
                             else  (NoPos1,NoHorizon,trade) // else rebalance or do nothing
                         | DateLimit datelimit ->
                             if current_date < datelimit 
                                then (pos,horizon,trade)
                             else (pos,NoHorizon,trade) // limit expires

    // EXECUTION THE STRATEGIES
    let d2 = mychart.Keys |> Seq.max
    let d1 = d2 - TimeSpan(30,0,0,0)

    let daterange = mychart.Keys |> Seq.filter (fun d -> d>=d1)

    // folder
                          
    let res = Seq.fold folder (NoPos,[]) mychart.Keys
    let trade_dates_bull = (snd res) |> List.map fst

    // folder1

    let res1 = Seq.fold folder1 (NoPos1,NoHorizon,[]) daterange
    let trade_dates_bull1 = (snd res1) |> List.map fst

    // folder1A
    let res1A = Seq.fold folder1A (NoPos1,NoHorizon,[]) daterange
    let ans:float list = res1A |> fun (a,b,c)->c |> List.map (fun (s,d,en,ex)-> (ex-en)/en)

    // PERFORMANCE of SIGNALS

    let printStats (str:string) (s:Series<DateTime,float>) = 
        printfn "%A mean = %A std = %A min = %A max = %A" (str) (Stats.mean s)(Stats.stdDev s)(Stats.min s)(Stats.max s)

    let getPCTwithFilter (filter:'T->bool) (s:Series<'K,'T>)  = 
        float ( (s |> Series.filterValues filter) |> Series.countKeys ) / float (s |> Series.countKeys )
    
    let S = exec_price_1

    let Sr10mn = (-Series.diff -2 S)/S // series of returns
    let res10mnbull = Series.getAll (trade_dates_bull) Sr10mn   

    let Sr6H = (-Series.diff -71 S)/S // series of returns
    let res6Hbull = Series.getAll (trade_dates_bull) Sr6H 

    printStats "Sr10mn"   Sr10mn
    printStats "res10mnbull"  res10mnbull

    printStats "Sr6H"  Sr6H
    printStats "res6Hbull" res6Hbull

    res6Hbull |> Chart.Bar

    let filter1 x = (x >= 0.1)
    getPCTwithFilter filter1 sr6
    getPCTwithFilter filter1 res6bull
    getPCTwithFilter filter1 sr24
    getPCTwithFilter filter1 res24bull

    let filter2 (x:float) = (x <= -0.1)
    getPCTwithFilter filter2 sr6
    getPCTwithFilter filter2 res6bull
    getPCTwithFilter filter2 sr24
    getPCTwithFilter filter2 res24bull

    let findMin s = 
        let a = Stats.minBy id s
        match a with
          | Some x -> fst x
          | None -> failwith "Error when finding min"

    let crashdate = findMin sr6

                                              
                                               
    Console.ReadLine() |> ignore

    0
 

    let plotHourlyAround (k:DateTime) =
        let k1 = k - TimeSpan(24,0,0)
        let k2 = k + TimeSpan(24,0,0)
        let ch = hourlychart.Between (k1,k2)
               |> unwrap_candle_series
               |> Series.map (fun d c -> match c with
                                          |(l,o,cl,h,v,w) -> (decimal l,decimal o,decimal cl,decimal h) )                                       
        [ for x in ch.Keys do let (l,o,cl,h) = ch.[x]
                              yield (x,l,o,cl,h) ]
       
            |> Chart.Candlestick       

    let plotHourlyAround_v1 (k:DateTime) =
        let k1 = k - TimeSpan(24,0,0)
        let k2 = k + TimeSpan(24,0,0)
        let ch = hourlychart.Between (k1,k2)
               |> unwrap_candle_series
               |> Series.map (fun d c -> match c with
                                          |(l,o,cl,h,v,w) -> (l,o,cl,h,v) )

        let candle = [ for x in ch.Keys do let (l,o,cl,h,v) = ch.[x]
                                           yield (x,l,o,cl,h) ]
        let volume = [ for x in ch.Keys do let (l,o,cl,h,v) = ch.[x]
                                           yield (x,0.0,0.0,-v,-v) ]

        let H = candle |> Seq.map (fun (a,b,c,d,e)->e    )|> Seq.max
        let L = candle |> Seq.map (fun (a,b,c,d,e)->b    )|> Seq.min
        let V = volume |> Seq.map (fun (a,b,c,d,e)->abs e)|> Seq.max

        let r = 1.0 // * V/H * 10.0

        let options2 =
                Options(
                   title = "Magic Trading " + string k,
                   vAxes = [|Axis(title = "Price");Axis(title="Volume")|],
                   hAxis = Axis(title = "DateTime"),
                   series = [|Series(targetAxisIndex=0);Series(targetAxisIndex=1)|]
                 )

        let vol_rescaled = volume |> List.map (fun (a,b,c,d,e)-> (a,b,c,d/r,e/r) )
        [candle;vol_rescaled]
            |> Chart.Candlestick
            |> Chart.WithOptions options2
 
    let plotHourlyAround_v2 (k:DateTime) =
        let k1 = k - TimeSpan(24,0,0)
        let k2 = k + TimeSpan(24,0,0)
        let ch = hourlychart.Between (k1,k2)
               |> unwrap_candle_series
               |> Series.map (fun d c -> match c with
                                          |(l,o,cl,h,v,w) -> (l,o,cl,h,v) )

        let line = [ for x in ch.Keys do let (l,o,cl,h,v) = ch.[x]
                                         yield (x,cl) ]
        let RSI = [ for x in ch.Keys -> (x,HourlyRSI.[x]) ]

        let H = line |> Seq.map (fun (a,b)->b  )|> Seq.max
        let L = line |> Seq.map (fun (a,b)->b    )|> Seq.min
        let R = RSI |> Seq.map (fun (a,b)->abs b)|> Seq.max

        let r = 1.0 // * R/H * 10.0

        let options2 =
                Options(
                   title = "Magic Trading " + string k,
                   vAxes = [|Axis(title = "Price");Axis(title="RSI")|],
                   hAxis = Axis(title = "DateTime"),
                   series = [|Series(targetAxisIndex=0);Series(targetAxisIndex=1)|]
                 )

        let RSI_rescaled = RSI |> List.map (fun (a,b)-> (a,b/r) )
        [line;RSI_rescaled]
            |> Chart.Line
            |> Chart.WithOptions options2


               
    // Useful shortcut in F# interactive
    let SC1A k =
        let keys = List.map (fun (a,b,c,d)->b) (res1A |> fun (a,b,c)->c)
        printfn "%A" (res1A |> fun (a,b,c)->c).[k]
        plotHourlyAround_v1 keys.[k]
  
    let SC2A k =
        let keys = List.map (fun (a,b,c,d)->b) (res1A |> fun (a,b,c)->c)
        printfn "%A" (res1A |> fun (a,b,c)->c).[k]
        plotHourlyAround_v2 keys.[k]
                             

    let k = crashdate
    let k = trade_dates_bull1.LastKey()

    plotHourlyAround k

    let keys = trade_dates_bull1.Keys |> Seq.toArray

    for k in [0..4] do plotHourlyAround keys.[k]


    // get a row when knowing the key
    df.GetRow<float> k1   

    // get the index of an observation 
    let addr = df.RowIndex.Locate(k1)
    df.GetRowsAt (int addr)         


   




    


let IsEven = Filter (fun x -> x % 2 = 0)
let IsDiv3 = Filter (fun x -> x % 3 = 0)

let myfilter = IsEven <>* IsDiv3
