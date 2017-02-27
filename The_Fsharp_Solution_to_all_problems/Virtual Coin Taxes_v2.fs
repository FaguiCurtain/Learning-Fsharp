namespace VirtualCoinTaxes

open System
open System.Collections.Generic
open System.IO

open FSharp.Data

module Definitions = 

   let Currencies = 
    [|"JPY";"BTC";"ETH";"EUR";"USD"|]

   let get_Currency_code s =
    Array.findIndex (fun e -> e=s) Currencies

   let num_currencies = Array.length Currencies
 

   type transaction_details = 
    {time     : DateTime ;
     pair     : string   ;
     size     : float    ;
     price    : float    ;
     fee      : float    ;
     }

   type transaction_quark_details =
    {time     : DateTime ;
     asset     : string   ;
     size     : float    ;
     jpyprice    : float    ;
     fee      : float    ;
     }


   type id = string // format "AB1234"
   type color = A |B
   type id1 = id * color

   type transaction_DB = Dictionary<id,transaction_details>
   type transaction = id * transaction_details
   type transaction_quark_DB = Dictionary<id1,transaction_details>

   type price_timeseries = Dictionary<DateTime,float> // (key = date, value = price)

module MyTransactions = 

   open Definitions
   // constructor

   let create_empty () = new transaction_DB()

   let (a:idd) = ("s", A)




   let compute_jpyprice (myT:transaction_DB)(jpy_priceseries:price_timeseries) id = 
       let tr_details = myT.[id]
       let d = tr_details.time
       myT.Remove(id) |> ignore
       let new_details = {tr_details with jpyprice=tr_details.price*jpy_priceseries.[d.Date]}
       myT.Add(id,new_details)
   let update_jpyprice (myT:transaction_DB)(jpy_priceseries:price_timeseries)(id_list:id list) =
       for id in id_list do
           compute_jpyprice myT jpy_priceseries id   
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
   type transaction_backlog =  (id * queue_details) list
   type taxable_event_details = {log: (id * id * t_size) ; // (first_transaction_id , second_transaction_id , size_used)
                            PL : float}
   type taxable_event = id * taxable_event_details //taxable_event_id * taxable_event_details
// let t:taxable_event = ("TE0001",{log=("ID0001","ID0003",50.0); PL=5000.0})
// let rec evolve_state0 (backlog : int list, taxableevent: int list, x:int) =
//    match backlog with 
//         | [] -> ([x],taxableevent)
//         |  _ -> if (sign(backlog.Head)=sign(x)) then (List.append backlog [x],taxableevent)
//                 else let a = backlog.Head
//                      if (a>0) then 
//                           if (a+x> 0) then ((a+x)::backlog.Tail,(-x)::taxableevent)
//                           elif (a+x =0) then  (backlog.Tail,(-x)::taxableevent)
//                           else evolve_state0 (backlog.Tail,(a::taxableevent),a+x)
//                      else 
//                           if (a+x< 0) then ((a+x)::backlog.Tail,(x::taxableevent))
//                           elif (a+x=0) then (backlog.Tail,x::taxableevent)
//                           else evolve_state0 (backlog.Tail,(-a)::taxableevent, a+x)

   let rec evolve_state1 (myT:transaction_DB)(backlog : (id*t_size) list,taxableevent: taxable_event list) (x:id*t_size) =
       let s2 = snd x
       let p2 = myT.[fst x].price
       let te_num= 
           match taxableevent with
             | [] -> "TE0000"
             |  _ -> fst taxableevent.Head
       match backlog with 
         | [] -> ([x],taxableevent)
         |  _ -> let s1 = snd backlog.Head 
                 let p1 = myT.[fst backlog.Head].price
      
                 if (sign(s1)=sign(s2)) then (List.append backlog [x],taxableevent)
                   else 

                      let te_num1 = increase_ID te_num
                      let id1 = fst backlog.Head
                      let id2 = fst x
                      if (s1>0.0) then 
                           
                           if (s1+s2> 0.0) then ( (id1,s1+s2)::backlog.Tail,(te_num1,{log=(id1,id2,-s2);PL=s2*(p1-p2)})::taxableevent)
                           elif (s1+s2 =0.0) then  (backlog.Tail,(te_num1,{log=(id1,id2,s1);PL=s2*(p1-p2)})::taxableevent)
                           else evolve_state1 myT ((backlog.Tail),((te_num1,{log=(id1,id2,s1);PL=s1*(p2-p1)})::taxableevent)) (id2,s1+s2)

                      else 
                           if (s1+s2< 0.0) then ( (id1,s1+s2)::backlog.Tail,(te_num1,{log=(id1,id2,-s2);PL=s2*(p1-p2)})::taxableevent)
                           elif (s1+s2 =0.0) then  (backlog.Tail,(te_num1,{log=(id1,id2,s1);PL=s2*(p1-p2)})::taxableevent)
                           else evolve_state1 myT ((backlog.Tail),((te_num1,{log=(id1,id2,s1);PL=s1*(p2-p1)})::taxableevent)) (id2,s1+s2)
                                                                           
   let make_taxable_event1 (myT:transaction_DB)(id_list:id list) = // makes sense only if all id are transactions in the same currency.
       let transaction_list:(id*t_size) list = [ for x in id_list -> (x,myT.[x].size) ]
       let rec myfun acc l = 
           match l with 
             | []   -> acc
             | e::t -> myfun (evolve_state1 myT acc e) t  // attention aux parenthèses !!!!
       myfun ([],[]) transaction_list 
// make_taxable_event1 ["ID0001";"ID0002";"ID0003";"ID0004"]
   let evolve_state2 (myT:transaction_DB)(multiccy_backlog: ((id*t_size) list) [],taxableevent:taxable_event list)(x:id*t_size)=
       let cur = myT.[fst x].pair
       let i = get_Currency_code cur
       let new_state_cur = evolve_state1 myT (multiccy_backlog.[i],taxableevent) x
       multiccy_backlog.[i] <- fst new_state_cur
       (multiccy_backlog,snd new_state_cur)
 
   let make_taxable_event2 (myT:transaction_DB)(id_list:id list) = 
       let transaction_list:(id*t_size) list = [ for x in id_list -> (x,myT.[x].size) ]
       let rec myfun acc l = 
           match l with 
             | []   -> acc
             | e::t -> myfun (evolve_state2 myT acc e) t  // attention aux parenthèses !!!!
       let empty_state = Array.create num_currencies []
       myfun (empty_state,[]) transaction_list 

   let all_transaction_id (myT:transaction_DB) = Seq.toList myT.Keys



// let x = File.ReadAllLines "/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/Algo Stanford/trades.csv"
   //let rawfile = CsvFile.Load("/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/Algo Stanford/trades.csv")

   //let buy_or_sell (order)=
   //    match order with
   //      | "buy"  -> 1.0
   //      | "sell" -> -1.0
   //      | _ -> 0.0       // should not happen

   //for row in rawfile.Rows do

   //    MyTransactions.Add(row.GetColumn("txid"),{time=System.DateTime.Parse (row.GetColumn("time"));
   //                                              pair=  row.GetColumn("pair");
   //                                              size=  float ( buy_or_sell(row.GetColumn("type")) * float (row.GetColumn("vol")));                                         
   //                                              price= float (row.GetColumn("price"));
   //                                              fee=   float (row.GetColumn("fee"))
   //                                              jpyprice= 0.0}    )

   //printfn "%f" (MyTransactions.["TRSYD5-F2NKZ-KHQ6WB"].size) // ligne 58 du fichier CSV 

   //let jpy = CsvFile.Load("http://ichart.finance.yahoo.com/table.csv?s=JPY=X").Cache()
   //let eur = CsvFile.Load("http://ichart.finance.yahoo.com/table.csv?s=EUR=X").Cache()

   //let jpy_priceseries1 = new price_timeseries()

   //[ for row in jpy.Rows -> System.DateTime.Parse (row.GetColumn("Date")), float (row.GetColumn("Close"))]
   //     |> List.filter (fun (d,p)-> d>=System.DateTime.Parse("01-11-2016"))
   //     |> List.iter (fun (d,p) -> jpy_priceseries1.Add(d,p))
 