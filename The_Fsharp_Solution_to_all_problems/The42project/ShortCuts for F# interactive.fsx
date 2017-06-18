///////////////////////////////////////

let filter1 x = (x >= 0.1)
getPCTwithFilter filter1 Sr6H
getPCTwithFilter filter1 res6Hbull
getPCTwithFilter filter1 Sr10mn
getPCTwithFilter filter1 res10mnbull

    //let filter2 (x:float) = (x <= -0.1)
    //getPCTwithFilter filter2 sr6
    //getPCTwithFilter filter2 res6bull
    //getPCTwithFilter filter2 sr24
    //getPCTwithFilter filter2 res24bull

/// finds the minimum of a series
let findMin s =
    let a = Stats.minBy id s
    match a with
       | Some x -> fst x
       | None -> failwith "Error when finding min"

// let crashdate = findMin sr6





////////////////////////////////////////////

let keys = ans |> Array.map (fun (a,b,c)->a) 

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

let plot5mnAround_v1 (k:DateTime) =
    let k1 = k - TimeSpan(6,0,0)
    let k2 = k + TimeSpan(6,0,0)
    let ch = _5mnchart.Between (k1,k2)
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
                   width = 2000,
                   height = 1200,
                   vAxes = [|Axis(title = "Price");Axis(title="Volume")|],
                   hAxis = Axis(title = "DateTime"),
                   series = [|Series(targetAxisIndex=0)
                             ;Series(targetAxisIndex=1)
                             |]
                 )

    let vol_rescaled = volume |> List.map (fun (a,b,c,d,e)-> (a,b,c,d/r,e/r) )
    [candle
     ;vol_rescaled
    ]
            |> Chart.Candlestick
            |> Chart.WithOptions options2





    // Useful shortcut in F# interactive
let SC1A k =
        printfn "%A" ans.[k]
        plotHourlyAround_v1 keys.[k]

let SC2A k =
        printfn "%A" ans.[k]
        plotHourlyAround_v2 keys.[k]

let SC11A k =
        printfn "%A" ans.[k]
        plot5mnAround_v1 keys.[k]

////////////////////////////////////////////////////


    let k = crashdate
    let k = trade_dates_bull1.LastKey()

    plotHourlyAround k

    let keys = trade_dates_bull1.Keys |> Seq.toArray

    for k in [0..4] do plotHourlyAround keys.[k]

    (res |> List.sum)/float (res |> List.length)


Console.ReadLine() |> ignore
