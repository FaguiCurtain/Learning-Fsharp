module PoloniexPublicAPI = 

    open System
    open System.IO
    open System.Net

    open DateTime_extratools


    /// Public API method to get Order Book
    /// https://poloniex.com/public?command=returnOrderBook&currencyPair=BTC_NXT&depth=10
    let make_getOrderBookReq currencyPair (depth:int) = 
        let commandType = "returnOrderBook"
        "https://poloniex.com/public?" + "command=" + commandType + "&currencyPair=" + currencyPair + "&depth=" + (string depth)

    /// Public API method to return candlestick chart data. 
    /// Required GET parameters are "currencyPair", "period" (candlestick period in seconds; valid values are 300, 900, 1800, 7200, 14400, and 86400), 
    /// "start", and "end". "Start" and "end" are given in UNIX timestamp format and used to specify the date range for the data returned. 
    /// https://poloniex.com/public?command=returnChartData&currencyPair=BTC_XMR&start=1405699200&end=9999999999&period=14400
   
    // 1405699200 = 19 july 2014

    let make_returnChartDataReq currencyPair start_date end_date period =
        let commandType1 = "returnChartData"
      //let currencyPair1 = "BTC_ETH"
      //let start_date1 = string 1405699200M // already defined up there
      //let end_date1   = string 9999999999M
      //let period1     = string 300M
      //let period1b    = string 14400M
        "https://poloniex.com/public?" + "command=" + commandType1 + "&currencyPair=" + currencyPair + "&start=" + (string start_date)
                      + "&end=" + (string end_date) + "&period=" + (string period)




    /// Get the contents of the URL via a web request
    let http (url: string) =
        let req = WebRequest.Create(url)
        let resp = req.GetResponse()
        let stream = resp.GetResponseStream()
        let reader = new StreamReader(stream)
        let html = reader.ReadToEnd()
        resp.Close()
        html
      
    /// helper function for GetPairHistory
    let CallAPI_and_save_JSON (webAPIcall:string,jsonFilename:string) = 
        let s = http webAPIcall
        File.WriteAllText (jsonFilename,s)

    /// writes on HD the price history of a given crypto-currency pair
    let GetPairHistory (pair:string) =
        let commandType1 = "returnChartData"
        let currencyPair1 = pair // for example "BTC_ETH"
        let start_date1 = 1405699200M // already defined up there
        let end_date1   = 9999999999M
        let period1     = 300M
        // let period1b    = 14400M

        let polReq = make_returnChartDataReq pair start_date1 end_date1 period1
        CallAPI_and_save_JSON (polReq,"/Users/francois-guillaume.rideau/Documents/crypto/trading/backtesting/"+pair+".json")

    /// hard-coded list of crypto-currencies
    // let Currency_list = ["BTC_ETH;""BTC_XRP";"BTC_ZEC"]
    // NO !!!!! cannot define a value in a module


    ////////////////
    // BOOK ORDER //
    ////////////////

    type Fill = 
        | TotalFill   of decimal * decimal
        | PartialFill of decimal * decimal 
    
    /// given a book Order and a size
    /// returns (executable size, average execution price)
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






