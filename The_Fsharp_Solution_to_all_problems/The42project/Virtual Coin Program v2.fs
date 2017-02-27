//Virtual Coin Taxes main program

module MainProgram

open VirtualCoinTaxes.Definitions
open VirtualCoinTaxes.MyTransactions

let debuginit (myT:transaction_DB)(jpy_priceseries:price_timeseries) = 

    let dt1 = System.DateTime.Parse "2016-01-13 16:19:47.606"
    let dt2 = System.DateTime.Parse "2016-02-13 16:19:47.606"
    let dt3 = System.DateTime.Parse "2016-03-13 16:19:47.606"
    let dt4 = System.DateTime.Parse "2016-04-13 16:19:47.606"

    myT.Add ("ID0001",{time=dt1;pair="BTC";size= -100.0;price=100.0;fee=2.0 ;jpyprice=0.0})
    myT.Add ("ID0002",{time=dt2;pair="BTC";size=  -50.0;price=120.0;fee=1.0 ;jpyprice=0.0})
    myT.Add ("ID0003",{time=dt3;pair="BTC";size= +50.0;price=200.0 ;fee=1.0 ;jpyprice=0.0})
    myT.Add ("ID0004",{time=dt4;pair="BTC";size= +50.0;price=200.0 ;fee=2.0 ;jpyprice=0.0})
    myT.Add ("ID0005",{time=dt1;pair="ETH";size= -100.0;price=10.0;fee=2.0 ;jpyprice=0.0})
    myT.Add ("ID0006",{time=dt2;pair="ETH";size=  -50.0;price=12.0;fee=1.0 ;jpyprice=0.0})
    myT.Add ("ID0007",{time=dt3;pair="ETH";size= +50.0;price=20.0 ;fee=1.0 ;jpyprice=0.0})
    myT.Add ("ID0008",{time=dt4;pair="ETH";size= +50.0;price=20.0 ;fee=2.0 ;jpyprice=0.0})

    printfn "%f" (myT.["ID0001"].size) 

    let dt11=System.DateTime.Parse "2016-01-13"
    let dt12=System.DateTime.Parse "2016-02-13"
    let dt13=System.DateTime.Parse "2016-03-13"
    let dt14=System.DateTime.Parse "2016-04-13"

    jpy_priceseries.Add(dt11,100.0)
    jpy_priceseries.Add(dt12,101.0)
    jpy_priceseries.Add(dt13,110.0)
    jpy_priceseries.Add(dt14,111.0)


[<EntryPoint>]
let main args =
   
       printfn "Arguments passed to function : %A" args

    //let stopWatch = System.Diagnostics.Stopwatch.StartNew()

       let MyT = VirtualCoinTaxes.MyTransactions.create_empty()
       let jpy_priceseries = new price_timeseries()

       debuginit MyT jpy_priceseries

       update_jpyprice (MyT)(jpy_priceseries)(all_transaction_id (MyT) )

       let res = make_taxable_event2 MyT (all_transaction_id MyT)

       printfn "%A" (fst res)
       printfn "%A" (snd res)



       0
    // Return 0. This indicates success.

