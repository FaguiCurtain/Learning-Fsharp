// how to load packages in F# interactive //

// add this folder to the library include path

// MAC
#I @"/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/packages"

// PC laptop
// #I @"C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\packages\"

// load the package in F# interactive
#r "FSharpx.Collections.1.15.2/lib/net40/FSharpx.Collections.dll"
//don't forget to type "open ..." in F# interactive after a #r 
open FSharpx.Collections

// https://www.nuget.org/packages/FSharp.Core
// FSharp.Core for F# 4.0 4.0.0.1
// Install-Package FSharp.Core

// PC Laptop
// #r "Spreads.0.3.5\lib\net45\Spreads.dll"
// #r "Spreads.Core.0.3.5\lib\net45\Spreads.Core.dll"
// #r "Spreads.Core.0.3.5\lib\net45\Spreads.Collections.dll"
// #r "Spreads.Extensions.0.3.5\lib\net45\Spreads.Extensions.dll"

#r "FSharp.Data.2.3.2/lib/net40/FSharp.Data.dll"

// Deedle
// #load "Deedle.1.2.5/Deedle.fsx"
#r "Deedle.1.2.5/lib/net40/Deedle.dll"
do fsi.AddPrinter(fun (printer:Deedle.Internal.IFsiFormattable) -> "\n" + (printer.Format()))

#r "Xplot.GoogleCharts.1.4.2/lib/net45/XPlot.GoogleCharts.dll"
#r "Xplot.GoogleCharts.Deedle.1.4.2/lib/net45/XPlot.GoogleCharts.Deedle.dll"
#r "Xplot.Plotly.1.4.2/lib/net45/Xplot.Plotly.dll"


#load "FSharp.Charting.Gtk.0.90.14/FSharp.Charting.Gtk.fsx"

// MATH.NET
#r "MathNet.Numerics.3.13.1/lib/net40/MathNet.Numerics.dll"
// #load "MathNet.Numerics/lib/net40/MathNet.Numerics.FSharp.dll" // n'existe plus ???
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
fsi.AddPrinter(fun (matrix:Matrix<float>) -> matrix.ToString())
fsi.AddPrinter(fun (matrix:Matrix<float32>) -> matrix.ToString())
fsi.AddPrinter(fun (matrix:Matrix<complex>) -> matrix.ToString())
fsi.AddPrinter(fun (matrix:Matrix<complex32>) -> matrix.ToString())
fsi.AddPrinter(fun (vector:Vector<float>) -> vector.ToString())
fsi.AddPrinter(fun (vector:Vector<float32>) -> vector.ToString())
fsi.AddPrinter(fun (vector:Vector<complex>) -> vector.ToString())
fsi.AddPrinter(fun (vector:Vector<complex32>) -> vector.ToString())
//

#load "FsLab.1.0.2/FsLab.fsx"
open System
open FSharp.Data
open Deedle
open XPlot.GoogleCharts
open XPlot.Plotly

// Newtonsoft.Json
#r "Newtonsoft.Json.9.0.1/lib/net40/Newtonsoft.Json.dll"
// Google.DataTable.Net.Wrapper
#r "Google.DataTable.Net.Wrapper.3.1.2.0/lib/Google.DataTable.Net.Wrapper.dll"



#load "RProvider.1.1.20/RProvider.fsx"
open RProvider
open RDotNet

let printSourceLocation() = 
     printfn "Line: %s" __SOURCE_DIRECTORY__
     printfn "Source Directory: %s" __SOURCE_DIRECTORY__
     printfn "Source File: %s" __SOURCE_FILE__
printSourceLocation()

// right click on references: add references > projects > solutions


//// converts each binary string to a integer 
//let parseLine (line:string)= 
//    int ("0b"+ line.Replace(" ",""))

// how to get the current directory in F# interactive
open System

Environment.CurrentDirectory
// Environment.CurrentDirectory <- "/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/"

[ for x in 0.0 .. 0.1 .. 3.14 -> 
    R.sin(x).GetValue<float>() ]
|> R.plot