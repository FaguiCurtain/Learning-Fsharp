let trace1 =
    Scatter(
        x = [1.; 2.; 3.],
        y = [40.; 50.; 60.],
        name = "yaxis data"
    )
let trace2 =
    Scatter(
        x = [1.; 2.; 3.],
        y = [4.; 5.; 6.],
        name = "yaxis2 data"
    )
let secondAxes =
    LinearAxis(
        overlaying = "y",
        side = "right"
    )
let layout = Layout(yaxis2 = secondAxes)

let data = [trace1; trace2] |> List.map(fun x -> x :> ITrace)

GenericChart.ofTraceObjects data layout

#I @"/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/packages"
#load "FsLab.1.0.2/FsLab.fsx"

open FSharp.Data
open Deedle
open XPlot.GoogleCharts

let myseq1 = [for i in 1..10 -> (i,float(i*i))]
let myseq2 = [for i in 1..10 -> (i,sin (float i))]
let myseq3 = [for i in 1..10 -> (i,10.0,12.0,14.0,15.0)]
let myseq4 = [for i in 1..10 -> (i,60.0+2.0*float i,60.0+2.0*float i,60.0+2.0*float i,60.0+2.0*float i)]
let myseq5 = [for i in 1..10 -> (i,0.0,-100.0-float i,-100.0-float i,-100.0-float i)]
let myseq5a =[for i in 1..10 -> (i,0.0,0.0,-100.0-float i,-100.0-float i)]

let options1 =
        Options(
           title = "trucmuche",
           vAxes = [|Axis(title = "Price");Axis(title="RSI")|],
           hAxis = Axis(title = "DateTime"),
           // seriesType = "bars",
           seriesType = "bars",
           series = [|Series(targetAxisIndex=0);Series(targetAxisIndex=1)|]
         )

[myseq1; myseq2] |> Chart.Combo
                 |> Chart.WithOptions options1

let options2 =
        Options(
           title = "trucmuche",
           vAxes = [|Axis(title = "Price");Axis(title="RSI")|],
           hAxis = Axis(title = "DateTime"),
           series = [|Series(targetAxisIndex=0);Series(targetAxisIndex=1);Series(targetAxisIndex=0)|]
         )

let H3 = myseq3  |> Seq.map (fun (a,b,c,d,e)-> abs e ) |> Seq.max
let H2 = myseq5a |> Seq.map (fun (a,b,c,d,e)-> abs d ) |> Seq.max

let r = H2/H3*3.0 //rescale_factor
let myseq5a_rescale = myseq5a |> Seq.map (fun (a,b,c,d,e) -> (a,b/r,c/r,d/r,e/r)) |> Seq.toList

[myseq3; myseq4;myseq5a_rescale] |> Chart.Candlestick
                                 |> Chart.WithOptions options2



 // multiple chart example with Plotly          


 open XPlot.Plotly
 let trace1' =
     Scatter(
         x = [0; 1; 2; 3; 4; 5],
         y = [1.5; 1.; 1.3; 0.7; 0.8; 0.9]
     ) :> Trace

 let trace2' =
     Bar(
         x = [0; 1; 2; 3; 4; 5],
         y = [1.; 0.5; 0.7; -1.2; 0.3; 0.4]
     ) :> Trace

 let multiLayout = Layout(title = "Line Chart and a Bar Chart")

 [trace1'; trace2']
 |> Chart.Plot
 |> Chart.WithLayout multiLayout
 |> Chart.WithWidth 700
 |> Chart.WithHeight 500