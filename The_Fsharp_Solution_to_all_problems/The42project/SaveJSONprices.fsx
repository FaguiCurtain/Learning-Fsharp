// MAC
#I @"/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/packages"

#load "FsLab.1.0.2/FsLab.fsx" // bug bizarre mais il faut le faire 2 fois dans F# interactive pour que ça marche


// executer DateTime_library.fs
// puis PoloniexPublicAPI_Libraries.fs

// à ne lancer qu'une seule fois dans F# interactive pour downloader les historiques de prix

open PoloniexPublicAPI

let Currency_list = ["BTC_ETH";"BTC_XRP";"BTC_ZEC"]

Currency_list |> List.iter GetPairHistory
printfn "ok"

let polReq = make_getOrderBookReq "BTC_ETH" 10

let start_date1 = 1405699200M // already defined up there
let end_date1   = 9999999999M
let period1b    = 14400M
let polReq1b = make_returnChartDataReq "BTC_ETH" start_date1 end_date1 period1b

// save a template once for all, can skip later
CallAPI_and_save_JSON (polReq ,"/Users/francois-guillaume.rideau/Documents/order_book.json")
CallAPI_and_save_JSON (polReq1b,"/Users/francois-guillaume.rideau/Documents/chart_data.json")

// open the Library to use the JsonProvider
open FSharp.Data

type ChartData = JsonProvider<"/Users/francois-guillaume.rideau/Documents/chart_data.json">
type ChartData = JsonProvider<"/Users/francois-guillaume.rideau/Documents/crypto/trading/backtesting/BTC_ZEC.json">



