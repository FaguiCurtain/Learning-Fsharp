open System

let x = File.ReadAllLines "/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/USDJPY.csv"

let y' = x |> Array.toList |> List.tail |> List.toArray
          |> Array.map (fun s -> s.[0..11]) 
          |> Array.map (System.DateTime.Parse)
          |> Array.map (fun (d:DateTime) -> d.Date)

let y  = Array.rev y'

let z = x |> Array.toList |> List.tail |> List.toArray
          |> Array.map (fun s -> s.[13..18]) 
          |> Array.map (fun text -> text.Replace (";",""))
          |> Array.map float
          |> Array.rev

for i in 0..(y.Length-1) do price_table.[USD].Add (y.[i],z.[i])

let dt2 = (System.DateTime.Parse "2016-02-13 16:19:47.606").Date

(Array.rev y) |> Array.find (fun s -> (s<= dt2) )

dt2.AddDays(1.0);;

let start_date = y.[0]
let end_date = y.[y.Length - 1]
let days_span = (end_date-start_date).Days
let full_dates = [for i in 0..days_span do yield start_date.AddDays(float i)]

full_dates |> List.iter (fun d -> let t = Array.find (fun s-> s<=d) y'
                                  if (t<>d) then price_table.[USD].Add(d,price_table.[USD].[t]) else ()
                         )