open Deedle

// series.diff : be careful of using it while calculating past or future returns

let myseq = [for i in 1..10 do yield (i,float (i*i))]
let myseries = Series.ofObservations myseq

Series.diff 1 myseries
 //val it : Series<int,int> =
 //2  -> 3  
 //3  -> 5  
 //4  -> 7  
 //5  -> 9  
 //6  -> 11 
 //7  -> 13 
 //8  -> 15 
 //9  -> 17 
 //10 -> 19 


(-Series.diff -6 myseries) // from a given key (date) get the future return from that point.
 //1 -> 48 
 //2 -> 60 
 //3 -> 72 
 //4 -> 84 

 Series.diff 6 myseries // from a given key (date) get the past return up to that point.
 //7  -> 48 
 //8  -> 60 
 //9  -> 72 
 //10 -> 84 

 // notice the sign change

 // playing with series

 let series1 = Series.ofObservations [(1,1);(2,2);(3,3)]
 let series2 = Series.ofObservations [(1,2);(2,2);(3,1);(4,4)]
 let series2b = Series.ofObservations [(1,2);(2,2);(3,1);(5,5)]

 let series1_plus_2 = series1+series2
  //val it : Series<int,int> =
  //1 -> 3         
  //2 -> 4         
  //3 -> 4         
  //4 -> <missing> 
 let series1_plus_2b = series1+series2b



 let series3 = series1.Zip(series2,JoinKind.Outer);;
 // the 'T opt type comes out of Zip etc....

  //val series3 : Series<int,(int opt * int opt)> =
  //1 -> (1, 2)         
  //2 -> (2, 2)         
  //3 -> (3, 1)         
  //4 -> (<missing>, 4) 

 let series3b = series1.Zip(series2,JoinKind.Inner);;
 let series3c = series1_plus_2.Zip(series1_plus_2b,JoinKind.Outer)

 let series4 = series3 |> Series.mapValues fst
  //val it : Series<int,int opt> =
  //1 -> 1         
  //2 -> 2         
  //3 -> 3         
  //4 -> <missing> 

 let series5 : Series<int,bool opt>=
    series4
       |> Series.mapValues(OptionalValue.map(fun x -> x > 1))
  //val it : Series<int,OptionalValue<bool>> =
  //1 -> False     
  //2 -> True      
  //3 -> True      
  //4 -> <missing> 


 let series6 =
     AvgHourlyClose
        |> Series.mapValues(OptionalValue.ofOption)

// don't forget the annotation else it would be Series<int,OptionalValue<int>
let myfun x = OptionalValue x
let series7:Series<int,int opt> = Series.mapValues (myfun) (series1)
 //val it : Series<int,OptionalValue<int>> = 
 //1 -> 1 
 //2 -> 2 
 //3 -> 3 


let map2series (f:'T1->'T2->'R)(series1:Series<'K,'T1 opt>)(series2:Series<'K,'T2 opt>):Series<'K,'R opt>=
     let S = series1.Zip(series2,JoinKind.Outer) //Series<'K,('T1 opt opt * 'T2 opt opt)>

     S |> Series.mapValues (fun (a,b) -> match (a,b) with 
                                          | (OptionalValue.Present(a'), OptionalValue.Present(b')) -> OptionalValue.map2 f a' b'
                                          | _ -> OptionalValue.Missing)
                           
/// converts a Series<'K,'T opt> to Series<'K,'T>
let convert series = 
    series |> Series.mapAll (fun _ v -> v |> Option.bind OptionalValue.asOption)


series4
  |> Series.filterValues(fun x -> x.HasValue)

let slidingsum_f (n:int) (series:Series<'K,float>) = 
    let rec helper k acc =
        if k <0 then failwith "error: negative argument"
                elif k=0 then acc
                else helper (k-1) (acc + series.Shift k)
    helper n series

slidingsum 11 Volume

let execute_with_stopwatch myasync =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    Async.RunSynchronously myasync
    stopWatch.Stop()
    printfn "executed in %f milliseconds" stopWatch.Elapsed.TotalMilliseconds

let myasync = async {let a=2
                     printfn "%A" a}


let some f =
    function
      | (OptionalValue.Present(a'), OptionalValue.Present(b')) -> Some (f a' b')
      | _ -> None

//val some : f:('a -> 'b -> 'c) -> 'a opt * 'b opt -> 'c option

let map2series2 f series1 =
     Series.zip series1
       >> Series.mapAll(fun k -> Option.bind(some f))

//val map2series2 :
   //f:('a -> 'b -> 'c) ->
     //series1:Series<'d,'a> -> (Series<'d,'b> -> Series<'d,'c>)
     //when 'd : equality

Option.bind;;
 //val it : (('a -> 'b option) -> 'a option -> 'b option)

map2series2 (+) series1 series2;;
 //val it : Series<int,int> =
 //1 -> 3         
 //2 -> 4         
 //3 -> 4         
 //4 -> <missing> 

let myfun = 
     let df = Frame.ofColumns["series1"=> series1;"series2"=>series2]
     0

 
let myseq = seq [("A",1);("B",2)]
Frame.ofRows myseq

myarray|> Array.map (fun (a,b)->(a,(b,c)))
       |> Array.toSeq
       |> Frame.ofRows;;

let root = "/Users/francois-guillaume.rideau/Documents/"

let msft = 
  Frame.ReadCsv(root + "MSFT.csv") 
  |> Frame.indexRowsDate "Date"
  |> Frame.sortRowsByKey