namespace VirtualCoinTaxes

open System
open System.Collections.Generic
open System.IO
open Microsoft.FSharp.Reflection

open FSharp.Data

module Definitions = 
   
   // naive way with arrays

   //let Currencies = 
   // [|"JPY";"BTC";"ETH";"EUR";"USD"|]

   //let get_Currency_code s =
   // Array.findIndex (fun e -> e=s) Currencies

   //let num_currencies = Array.length Currencies
 
   // more F# like

   type Currency = JPY | BTC     | ETH    | EUR    | USD 
                       | BTCUSD  | ETHBTC | EURUSD
                       | ARDR    | BTS    | DAO    | DGD    | ETC    | FCT    | LTC    | MAID    | NXT    | REP    | SC    | SDC    | SYS    | XMR    | XRP
                       | ARDRBTC | BTSBTC | DAOETH | DGDBTC | ETCBTC | FCTBTC | LTCBTC | MAIDBTC | NXTBTC | REPBTC | SCBTC | SDCBTC | SYSBTC | XMRBTC | XRPBTC
     
                      with
        member this.toString =
           match this with
                 | JPY    -> "JPY"
                 | BTC    -> "BTC"
                 | ETH    -> "ETH"
                 | EUR    -> "EUR"
                 | USD    -> "USD"
                 | BTCUSD -> "BTCUSD"
                 | ETHBTC -> "ETHBTC"
                 | EURUSD -> "EURUSD"              
                 | ARDR   -> "ARDR"
                 | BTS    -> "BTS"
                 | DAO    -> "DAO"
                 | DGD    -> "DGD"
                 | ETC    -> "ETC"
                 | FCT    -> "FCT"
                 | LTC    -> "LTC"
                 | MAID   -> "MAID"
                 | NXT    -> "NXT"
                 | REP    -> "REP"
                 | SC     -> "SC"
                 | SDC    -> "SDC"
                 | SYS    -> "SYS"
                 | XMR    -> "XMR"
                 | XRP    -> "XRP"
                 | ARDRBTC-> "ARDRBTC"
                 | BTSBTC -> "BTSBTC"
                 | DAOETH -> "DAOETH"
                 | DGDBTC -> "DGDBTC"
                 | ETCBTC -> "ETCBTC"
                 | FCTBTC -> "FCTBTC"
                 | LTCBTC -> "LTCBTC"
                 | MAIDBTC-> "MAIDBTC"
                 | NXTBTC -> "NXTBTC"
                 | REPBTC -> "REPBTC"
                 | SCBTC  -> "SCBTC"
                 | SDCBTC -> "SDCBTC"
                 | SYSBTC -> "SYSBTC"
                 | XMRBTC -> "XMRBTC"
                 | XRPBTC -> "XRPBTC"
 
   //let  Currency_list = [JPY     ; BTC     ; ETH    ; EUR    ; USD   ;
   //                                BTCUSD  ; ETHBTC ; EURUSD ;
   //                      ARDR    ; BTS     ; DAO    ; ETC    ; FCT    ; LTC    ; MAID    ; NXT    ; REP    ; SC    ; SDC    ; SYS    ; XMR    ; XRP   ;
   //                      ARDRBTC ; BTSBTC  ; DAOETH ; ETCBTC ; FCTBTC ; LTCBTC ; MAIDBTC ; NXTBTC ; REPBTC ; SCBTC ; SDCBTC ; SYSBTC ; XMRBTC ; XRPBTC ]

   let allUnionCases<'T>() =
       FSharpType.GetUnionCases(typeof<'T>)
       |> Array.map (fun case -> FSharpValue.MakeUnion(case, [||]) :?> 'T)

   // let  C = FSharpType.GetUnionCases typeof<Currency> |> Array.toList // not exactly what I need
   let Currency_list = allUnionCases<Currency>() |> Array.toList

   type transaction_details = 
    {time     : DateTime ;
     pair     : Currency * Currency  ;
     size     : float    ;
     price    : float    ;
     fee      : float    ;
     }

   type transaction_quark_details =
    {time     : DateTime ;
     currency : Currency ;
     size     : float    ;
     jpyprice : float    ;
     fee      : float    ;
     }

   type id = string // format "AB1234"
   type color = A |B with
        member this.toString =
            match this with
              |A -> "A"
              |B -> "B"
   type id1 = id * color

   type transaction_DB = Dictionary<id,transaction_details>
   type transaction = id * transaction_details
   type transaction_quark_DB = Dictionary<id1,transaction_quark_details>

   type price = float
   type price_timeseries = Dictionary<DateTime,price> // (key = date, value = price)
   type price_table = Dictionary<Currency,price_timeseries>

module PriceTable = 
    open Definitions
    //constructor
    let create_empty() = new price_table()
    let create_empty_series() = new price_timeseries()

    /// initialize a price table with empty series for each available currency
    let init() = 
               let d = create_empty() 
               for cur in Currency_list do d.Add(cur,create_empty_series())
               d
    
    /// get the Daily price of a currency at a given date
    let get_currency_price (price_table:price_table) (cur,time:DateTime) =
        (price_table.[cur]).[time.Date]
    
    /// converts a date in UNIX POSIX format into DateTime
    let toDateTime (timestamp:int) =
        let start = DateTime(1970,1,1,0,0,0,DateTimeKind.Utc)
        start.AddSeconds(float timestamp).ToLocalTime()
    
    /// computes the price series of the 2nd currency from the first one. price_table.[cur2] must be empty before this call
    let map_series (price_table:price_table)(cur1:Currency,cur2:Currency)(f: float -> float)=
        let K = price_table.[cur1].Keys |> Seq.toList
        for t in K do
            price_table.[cur2].Add (t,f (price_table.[cur1].[t]))

    /// re-computes the price series of the currency
    let remap_series (price_table:price_table)(cur:Currency)(f: float -> float)=
        let K = price_table.[cur].Keys |> Seq.toList
        let mutable tmp = 0.0
        for t in K do
            let tmp = f (price_table.[cur].[t])
            price_table.[cur].[t] <- tmp
    
    /// compute the priceseries of the 3rd currency from the first two. price_table.[cur3] must be empty before this call
    let cross_2_series (price_table:price_table)(cur1:Currency,cur2:Currency,cur3:Currency)(f: float -> float -> float) =
        let K1 = price_table.[cur1].Keys |> Seq.toList
        let K2 = price_table.[cur2].Keys |> Seq.toList
        let K = Set.intersect (set K1) (set K2) |> Set.toList

        for t in K do
            let a = price_table.[cur1].[t]
            let b = price_table.[cur2].[t]
            price_table.[cur3].Add (t,f a b)
       

    let multiply_float (a:float) (b:float) = a * b
    let divide_float   (a:float) (b:float) = a / b
               
module MyTransactions = 

   open Definitions
   open PriceTable

   // constructor

   let create_empty () = new transaction_DB()

   /// breaks a pair transaction (buy CUR1, sell CUR2) into (buy CUR1,sell JPY)+(sell CUR2, buy JPY) transaction for accounting purposes
   let break_into_quark (myT:transaction)(price_table:price_table) =
       let id = fst myT
       let tr_d=snd myT

       let t = tr_d.time
       let (a,b) = tr_d.pair
       let p = tr_d.price

       ( ((id,A),{time = t;
                  currency = a;
                  size = tr_d.size;
                  jpyprice = p * (get_currency_price (price_table)(b,t) )
                  fee = tr_d.fee*get_currency_price (price_table)(EUR,t)}),
         ((id,B),{time = t;
                  currency = b;
                  size = -tr_d.size * p;
                  jpyprice = (get_currency_price (price_table)(b,t) )
                  fee = 0.0}) )

   /// breaks transactions for the whole transaction DataBase     
   let make_quark (myT: transaction_DB)(price_table:price_table) = 
        let dict = new transaction_quark_DB()
        for id in myT.Keys do
            let (t1,t2) = break_into_quark (id,myT.[id]) price_table
            dict.Add(fst t1,snd t1)
            dict.Add(fst t2,snd t2)
        dict

   /// generate the next ID
   let increase_ID (id_num:string) = 
       let s =string((int id_num.[2..5])+1)
       let pre = id_num.[0..1]
       let len = s.Length
       match len with 
         | 1 -> pre+"000"+ s
         | 2 -> pre+"00" + s
         | 3 -> pre+"0"  + s
         | 4 -> pre+ s
         | _ -> "" // should not happen

   type t_size = float
   type queue_details = {size_left : float}
   type transaction_backlog =  (id1 * queue_details) list
   type taxable_event_details = {log: (id1 * id1 * t_size) ; // (first_transaction_id , second_transaction_id , size_used)
                            PL : float}
   type taxable_event = id * taxable_event_details //taxable_event_id * taxable_event_details

   ////// first-in first-out method //////

   /// first-in first-out method helper function. one currency
   let rec evolve_state1 (myT:transaction_quark_DB)(backlog : (id1*t_size) list,taxableevent: taxable_event list) (x:id1*t_size) =
       let s2 = snd x
       let p2 = (myT.[fst x]).jpyprice
       let te_num= 
           match taxableevent with
             | [] -> "TE0000"
             |  _ -> fst taxableevent.Head
       match backlog with 
         | [] -> ([x],taxableevent)
         |  _ -> let s1 = snd backlog.Head 
                 let p1 = myT.[fst backlog.Head].jpyprice
      
                 if (sign(s1)=sign(s2)) then (List.append backlog [x],taxableevent)
                   else 

                      let te_num1 = increase_ID te_num
                      let id1 = fst backlog.Head
                      let id2 = fst x
                      if (s1>0.0) then // en fait ça ne dépend pas du signe de s1, mais je laisse pour réflexion...
                           
                           if (s1+s2> 0.0) then ( (id1,s1+s2)::backlog.Tail,(te_num1,{log=(id1,id2,-s2);PL=s2*(p1-p2)})::taxableevent)
                           elif (s1+s2 =0.0) then  (backlog.Tail,(te_num1,{log=(id1,id2,s1);PL=s2*(p1-p2)})::taxableevent)
                           else evolve_state1 myT ((backlog.Tail),((te_num1,{log=(id1,id2,s1);PL=s1*(p2-p1)})::taxableevent)) (id2,s1+s2)

                      else 
                           if (s1+s2< 0.0) then ( (id1,s1+s2)::backlog.Tail,(te_num1,{log=(id1,id2,-s2);PL=s2*(p1-p2)})::taxableevent)
                           elif (s1+s2 =0.0) then  (backlog.Tail,(te_num1,{log=(id1,id2,s1);PL=s2*(p1-p2)})::taxableevent)
                           else evolve_state1 myT ((backlog.Tail),((te_num1,{log=(id1,id2,s1);PL=s1*(p2-p1)})::taxableevent)) (id2,s1+s2)
                                                                           
   let make_taxable_event1 (myT:transaction_quark_DB)(id1_list:id1 list) = // makes sense only if all id are transactions in the same currency.
       let transaction_list:(id1*t_size) list = [ for x in id1_list -> (x,myT.[x].size) ]
       let rec myfun acc l = 
           match l with 
             | []   -> acc
             | e::t -> myfun (evolve_state1 myT acc e) t  // attention aux parenthèses !!!!
       myfun ([],[]) transaction_list 


   let evolve_state2 (myT:transaction_quark_DB)(multiccy_backlog: Dictionary<Currency,(id1*t_size) list> ,taxableevent_log:Dictionary<Currency,taxable_event list>)(x:id1*t_size)=
       let cur = myT.[fst x].currency

       let new_state_cur = evolve_state1 myT (multiccy_backlog.[cur],taxableevent_log.[cur]) x
       multiccy_backlog.[cur] <- fst new_state_cur
       taxableevent_log.[cur] <- snd new_state_cur
       (multiccy_backlog,taxableevent_log)
 
   let make_taxable_event2 (myT:transaction_quark_DB)(id1_list:id1 list) = 
       let transaction_list:(id1*t_size) list = [ for x in id1_list -> (x,myT.[x].size) ]
       let rec myfun acc l = 
           match l with 
             | []   -> acc
             | e::t -> myfun (evolve_state2 myT acc e) t  // attention aux parenthèses !!!!
       let empty_state    = let d = new Dictionary<Currency,(id1*t_size) list>()
                            for cur in Currency_list do d.Add(cur,[])
                            d
       let empty_tax_log  = let d = new Dictionary<Currency,taxable_event list>()
                            for cur in Currency_list do d.Add(cur,[])
                            d
       myfun (empty_state,empty_tax_log) transaction_list 
   
   let make_one_summary (taxableevent:taxable_event list) = 
       taxableevent |> List.fold (fun acc event -> acc + (snd event).PL) 0.0

   let make_all_summary (taxableeventlog:Dictionary<Currency,taxable_event list>) =
       let res = new Dictionary<Currency,float>()
       for cur in Currency_list do
           let tmp = make_one_summary taxableeventlog.[cur]
           if tmp <> 0.0 then printfn "%A %A" cur tmp
           res.[cur] <- tmp
       let mutable acc=0.0
       for cur in Currency_list do acc<-acc + res.[cur]
       printfn "Total PL = %A" acc
       res
   
   ////// average method //////

   /// average method helper function. one currency
   type position  = {size:t_size;price:price}
   type minitrade  = {id1:id1;size:t_size;price:price}

   let rec evolve_state3 (myT:transaction_quark_DB)(position : position,taxableevent: taxable_event list) (x:minitrade) : (position*taxable_event list)=
       let s2 = x.size
       let p2 = x.price
       let te_num= 
           match taxableevent with
             | [] -> "TE0000"
             |  _ -> fst taxableevent.Head
       let s1 = position.size
       match s1 with 
         | 0.0 -> ({size=s2;price=p2},taxableevent)
         |  _  -> let p1 = position.price
                  if (sign(s1)=sign(s2)) then ({size=s1+s2;price=(p1*s1+p2*s2)/(s1+s2)},taxableevent)
                    else 

                      let te_num1 = increase_ID te_num

                      let id2 = x.id1
                      let id1 = id2 // corriger plus tard en créant un nouveau type
                      if (s1>0.0) then 
                           
                           if (s1+s2> 0.0)   then ( {size=s1+s2;price=p1},(te_num1,{log=(id1,id2,-s2);PL=s2*(p1-p2)})::taxableevent)
                           elif (s1+s2 =0.0) then ( {size=0.0  ;price=p2},(te_num1,{log=(id1,id2,s1) ;PL=s2*(p1-p2)})::taxableevent)
                           else                   ( {size=s1+s2;price=p2},(te_num1,{log=(id1,id2,s1) ;PL=s1*(p2-p1)})::taxableevent)

                      else 
                           if (s1+s2< 0.0)   then ( {size=s1+s2;price=p1},(te_num1,{log=(id1,id2,-s2);PL=s2*(p1-p2)})::taxableevent)
                           elif (s1+s2 =0.0) then ( {size=0.0  ;price=p2},(te_num1,{log=(id1,id2,s1) ;PL=s2*(p1-p2)})::taxableevent)
                           else                   ( {size=s1+s2;price=p2},(te_num1,{log=(id1,id2,s1) ;PL=s1*(p2-p1)})::taxableevent)
                                                                           
   let make_taxable_event3 (myT:transaction_quark_DB)(id1_list:id1 list) = // makes sense only if all id are transactions in the same currency.
       let transaction_list:minitrade list = [ for x in id1_list -> {id1=x;size=myT.[x].size;price=myT.[x].jpyprice} ]
       let rec myfun acc l = 
           match l with 
             | []   -> acc
             | e::t -> myfun (evolve_state3 myT acc e) t  // attention aux parenthèses !!!!
       let (empty_acc:position*taxable_event list) = ( {size=0.0;price=1.0} ,[])
       myfun (empty_acc) transaction_list 

   let evolve_state4 (myT:transaction_quark_DB)(multiccy_position: Dictionary<Currency,position> ,taxableevent_log:Dictionary<Currency,taxable_event list>)(x:minitrade)=
        let cur = myT.[x.id1].currency

        let new_state_cur = evolve_state3 myT (multiccy_position.[cur],taxableevent_log.[cur]) x
        multiccy_position.[cur] <- fst new_state_cur
        taxableevent_log.[cur] <- snd new_state_cur
        (multiccy_position,taxableevent_log)
  
   let make_taxable_event4 (myT:transaction_quark_DB)(id1_list:id1 list) = 
        let transaction_list:minitrade list = [ for x in id1_list -> {id1=x;size=myT.[x].size;price=myT.[x].jpyprice} ]
        let rec myfun acc l = 
            match l with 
              | []   -> acc
              | e::t -> myfun (evolve_state4 myT acc e) t  // attention aux parenthèses !!!!
        let empty_position = let d = new Dictionary<Currency,position>()
                             for cur in Currency_list do d.Add(cur,{size = 0.0; price = -1.0 })
                             d
        let empty_tax_log  = let d = new Dictionary<Currency,taxable_event list>()
                             for cur in Currency_list do d.Add(cur,[])
                             d
        myfun (empty_position,empty_tax_log) transaction_list 

   /// sort dates of transaction_quark_DB in order
   let get_sort_id1_list_by_date (myT:transaction_quark_DB)=
       myT.Keys |> Seq.toList // this is not necessarily sorted
                |> List.sortBy (fun id1 -> (myT.[id1]).time.Date)

   /// filter id_list for transaction_DB
   let filter_id_list (filter: transaction_DB -> id -> bool)(myT:transaction_DB)(id_list:id list) =
       let keys = myT.Keys |> Seq.toList
       for id in keys do if (filter myT id = false) then (myT.Remove(id) |> ignore)
    
   /// filter id1_list for transaction_quark_DB
   let filter_id1_list (filter: transaction_quark_DB -> id1 -> bool)(myT:transaction_quark_DB)(id1_list:id1 list) =
        let keys = myT.Keys |> Seq.toList
        for id1 in keys do if (filter myT id1 = false) then (myT.Remove(id1) |> ignore) 

   /// pseudofold on Dictionary - no dependency on key
   let dictpseudofold (folder:('a -> 'b -> 'a)) (acc:'a) (dict:Dictionary<'K,'b>) =
       let mutable a =acc
       for k in (dict.Keys |> Seq.toList) do
           a <- folder a dict.[k]
       a

   /// get the sum of notionals of all transactions in a dictionary (should be filtered before to make sense)
   let get_size_sum (D:transaction_quark_DB) =
       let folder x (tr_q_d:transaction_quark_details) =
           x + tr_q_d.size
       dictpseudofold folder 0.0 D

   /// get the latent profit of a position
   let latent_total_profit (pos:Dictionary<Currency,position>)(price_table:price_table)(date:DateTime)=
       let d = date.Date
       let K = pos.Keys |> Seq.toList

       let mutable acc   = 0.0
       let mutable acc1  = 0.0
       for cur in K do
           let size = pos.[cur].size
           if size <>0.0 then 
               let tmp = size * (price_table.[cur].[d] - pos.[cur].price)
               let tmp1  = size * price_table.[cur].[d]
               printfn "latent profit in %A = %A" cur tmp
               printfn "size = %A, avg price = %A, final price = %A" (pos.[cur].size) (pos.[cur].price) (price_table.[cur].[d])
               printfn "valuation = %A" ((pos.[cur].size)*(price_table.[cur].[d]))
               acc  <- acc+tmp
               acc1 <- acc1 + tmp1
       printfn "total latent profit = %A" acc
       printfn "total valuation     = %A" acc1



