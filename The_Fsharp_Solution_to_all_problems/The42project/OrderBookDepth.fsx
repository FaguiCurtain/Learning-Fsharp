// MAC
#I @"/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/packages"

#load "FsLab.1.0.2/FsLab.fsx" // bug bizarre mais il faut le faire 2 fois dans F# interactive pour que ça marche

open FSharp.Data

open PoloniexPublicAPI

type BookOrder = JsonProvider<"/Users/francois-guillaume.rideau/Documents/order_book.json">

// TEST ORDER BOOK

    (* BookOrderNow.Bids example for BTCETH
 
    val it: decimal [] []
    [|   [|0.04174999M; 1.899076M|]; [|0.04175000M; 1.9M|];
         [|0.04175100M; 7.49640255M|]; [|0.04175200M; 10M|];
         [|0.04177898M; 30.372M|]; [|0.04177900M; 69.9284M|];
         [|0.04178498M; 0.00616378M|]; [|0.04178899M; 185.21624954M|];
         [|0.04178900M; 143.203M|]; [|0.04179970M; 2M|]|]

    the best offer is at the price of 1 ETH = 0.04174999 BTC for an amount of 1.899076 ETH *)

let myBookOrder = [| [|1M;10M|];[|2M;10M|];[|3M;20M|] |]

let polReq = make_getOrderBookReq "BTC_ETH" 10
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

getJsonContent BookOrderNow "bids"

    
printfn "%A %A %A" (executable_price myBookOrder 20M)(executable_price myBookOrder 30M)(executable_price myBookOrder 50M)
