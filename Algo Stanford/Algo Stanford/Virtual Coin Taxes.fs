open System
open System.Collections.Generic
open System.IO

open FSharp.Data

let stopWatch = System.Diagnostics.Stopwatch.StartNew()

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
     jpyprice : float}

type id = string // format "AB1234"
type transaction_DB = Dictionary<id,transaction_details>
type transaction = id * transaction_details

let MyTransactions = new transaction_DB()

let dt1 = System.DateTime.Parse "2016-01-13 16:19:47.606"
let dt2 = System.DateTime.Parse "2016-02-13 16:19:47.606"
let dt3 = System.DateTime.Parse "2016-03-13 16:19:47.606"
let dt4 = System.DateTime.Parse "2016-04-13 16:19:47.606"

MyTransactions.Add ("ID0001",{time=dt1;pair="BTC";size= -100.0;price=100.0;fee=2.0 ;jpyprice=0.0})
MyTransactions.Add ("ID0002",{time=dt2;pair="BTC";size=  -50.0;price=120.0;fee=1.0 ;jpyprice=0.0})
MyTransactions.Add ("ID0003",{time=dt3;pair="BTC";size= +50.0;price=200.0 ;fee=1.0 ;jpyprice=0.0})
MyTransactions.Add ("ID0004",{time=dt4;pair="BTC";size= +50.0;price=200.0 ;fee=2.0 ;jpyprice=0.0})
MyTransactions.Add ("ID0005",{time=dt1;pair="ETH";size= -100.0;price=10.0;fee=2.0 ;jpyprice=0.0})
MyTransactions.Add ("ID0006",{time=dt2;pair="ETH";size=  -50.0;price=12.0;fee=1.0 ;jpyprice=0.0})
MyTransactions.Add ("ID0007",{time=dt3;pair="ETH";size= +50.0;price=20.0 ;fee=1.0 ;jpyprice=0.0})
MyTransactions.Add ("ID0008",{time=dt4;pair="ETH";size= +50.0;price=20.0 ;fee=2.0 ;jpyprice=0.0})



printfn "%f" (MyTransactions.["ID0001"].size) 


let dt11=System.DateTime.Parse "2016-01-13"
let dt12=System.DateTime.Parse "2016-02-13"
let dt13=System.DateTime.Parse "2016-03-13"
let dt14=System.DateTime.Parse "2016-04-13"

type price_timeseries = Dictionary<DateTime,float> // (key = date, value = price)
let jpy_priceseries = new price_timeseries()
jpy_priceseries.Add(dt11,100.0)
jpy_priceseries.Add(dt12,101.0)
jpy_priceseries.Add(dt13,110.0)
jpy_priceseries.Add(dt14,111.0)

let compute_jpyprice (id) = 
    let tr_details = MyTransactions.[id]
    let d = tr_details.time
    MyTransactions.Remove(id) |> ignore
    let new_details = {tr_details with jpyprice=tr_details.price*jpy_priceseries.[d.Date]}
    MyTransactions.Add(id,new_details)

let update_jpyprice (id_list:id list) =
    for id in id_list do
        compute_jpyprice id

   


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


//let rec evolve_state0 (backlog : int list, taxableevent: int list, x:int) =
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

let rec evolve_state1 (backlog : (id*t_size) list,taxableevent: taxable_event list) (x:id*t_size) =
    let s2 = snd x
    let p2 = MyTransactions.[fst x].price

    let te_num= 
        match taxableevent with
          | [] -> "TE0000"
          |  _ -> fst taxableevent.Head

    match backlog with 
         | [] -> ([x],taxableevent)
         |  _ -> let s1 = snd backlog.Head 
                 let p1 = MyTransactions.[fst backlog.Head].price
         
                 if (sign(s1)=sign(s2)) then (List.append backlog [x],taxableevent)
                 else 

                      let te_num1 = increase_ID te_num
                      let id1 = fst backlog.Head
                      let id2 = fst x
                      if (s1>0.0) then 
                           
                           if (s1+s2> 0.0) then ( (id1,s1+s2)::backlog.Tail,(te_num1,{log=(id1,id2,-s2);PL=s2*(p1-p2)})::taxableevent)
                           elif (s1+s2 =0.0) then  (backlog.Tail,(te_num1,{log=(id1,id2,s1);PL=s2*(p1-p2)})::taxableevent)
                           else evolve_state1 ((backlog.Tail),((te_num1,{log=(id1,id2,s1);PL=s1*(p2-p1)})::taxableevent)) (id2,s1+s2)

                      else 
                           if (s1+s2< 0.0) then ( (id1,s1+s2)::backlog.Tail,(te_num1,{log=(id1,id2,-s2);PL=s2*(p1-p2)})::taxableevent)
                           elif (s1+s2 =0.0) then  (backlog.Tail,(te_num1,{log=(id1,id2,s1);PL=s2*(p1-p2)})::taxableevent)
                           else evolve_state1 ((backlog.Tail),((te_num1,{log=(id1,id2,s1);PL=s1*(p2-p1)})::taxableevent)) (id2,s1+s2)
                                                                              

let make_taxable_event1 (id_list:id list) = // makes sense only if all id are transactions in the same currency.
    let transaction_list:(id*t_size) list = [ for x in id_list -> (x,MyTransactions.[x].size) ]
    let rec myfun acc l = 
        match l with 
           | []   -> acc
           | e::t -> myfun (evolve_state1 acc e) t  // attention aux parenthèses !!!!
    myfun ([],[]) transaction_list 

// make_taxable_event1 ["ID0001";"ID0002";"ID0003";"ID0004"]


let evolve_state2 (multiccy_backlog: ((id*t_size) list) [],taxableevent:taxable_event list)(x:id*t_size)=
    let cur = MyTransactions.[fst x].pair
    let i = get_Currency_code cur
    let new_state_cur = evolve_state1(multiccy_backlog.[i],taxableevent) x
    multiccy_backlog.[i] <- fst new_state_cur
    (multiccy_backlog,snd new_state_cur)
 
let make_taxable_event2 (id_list:id list) = 
    let transaction_list:(id*t_size) list = [ for x in id_list -> (x,MyTransactions.[x].size) ]
    let rec myfun acc l = 
        match l with 
           | []   -> acc
           | e::t -> myfun (evolve_state2 acc e) t  // attention aux parenthèses !!!!
    let empty_state = Array.create num_currencies []
    myfun (empty_state,[]) transaction_list 

let all_transaction_id = Seq.toList MyTransactions.Keys
update_jpyprice all_transaction_id

make_taxable_event2 all_transaction_id |> ignore


// let x = File.ReadAllLines "/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/Algo Stanford/trades.csv"
let rawfile = CsvFile.Load("/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/Algo Stanford/trades.csv")

let buy_or_sell (order)=
    match order with
       | "buy"  -> 1.0
       | "sell" -> -1.0
       | _ -> 0.0       // should not happen

for row in rawfile.Rows do

    MyTransactions.Add(row.GetColumn("txid"),{time=System.DateTime.Parse (row.GetColumn("time"));
                                              pair=  row.GetColumn("pair");
                                              size=  float ( buy_or_sell(row.GetColumn("type")) * float (row.GetColumn("vol")));                                         
                                              price= float (row.GetColumn("price"));
                                              fee=   float (row.GetColumn("fee"))
                                              jpyprice= 0.0}    )

printfn "%f" (MyTransactions.["TRSYD5-F2NKZ-KHQ6WB"].size) // ligne 58 du fichier CSV 

let jpy = CsvFile.Load("http://ichart.finance.yahoo.com/table.csv?s=JPY=X").Cache()
let eur = CsvFile.Load("http://ichart.finance.yahoo.com/table.csv?s=EUR=X").Cache()

let jpy_priceseries1 = new price_timeseries()

[ for row in jpy.Rows -> System.DateTime.Parse (row.GetColumn("Date")), float (row.GetColumn("Close"))]
      |> List.filter (fun (d,p)-> d>=System.DateTime.Parse("01-11-2016"))
      |> List.iter (fun (d,p) -> jpy_priceseries1.Add(d,p))
 