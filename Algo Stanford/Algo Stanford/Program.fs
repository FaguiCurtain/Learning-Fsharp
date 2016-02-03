// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open FSharp.Data
let msft = CsvFile.Load("http://ichart.finance.yahoo.com/table.csv?s=MSFT").Cache()
for row in msft.Rows do
  printfn "HLOC: (%s, %s, %s)" (row.GetColumn "High") (row.GetColumn "Low") (row.GetColumn "Date")

open System.IO
let x = File.ReadAllLines "C:\Users\Fagui\Documents\coursera\Algorithms\Stanford I\PA 3 - kargerMinCut.txt";;


// firstRow;;

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
