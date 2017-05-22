// how to load packages in F# interactive //

// PC laptop
// #r @"C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\packages\FSharpx.Collections.1.13.4\lib\net40\FSharpx.Collections.dll"
#r @"/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/packages/FSharpx.Collections.1.15.2/lib/net40/FSharpx.Collections.dll"

//don't forget to type in F# interactive

open FSharpx.Collections

// https://www.nuget.org/packages/FSharp.Core
// FSharp.Core for F# 4.0 4.0.0.1
// Install-Package FSharp.Core

// PC Laptop
// #r @"C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\packages\Spreads.0.3.5\lib\net45\Spreads.dll"
// #r @"C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\packages\Spreads.Core.0.3.5\lib\net45\Spreads.Core.dll"
// #r @"C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\packages\Spreads.Core.0.3.5\lib\net45\Spreads.Collections.dll"
// #r @"C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\packages\Spreads.Extensions.0.3.5\lib\net45\Spreads.Extensions.dll"

// #r @"C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\packages\FSharp.Data.2.3.2\lib\net40\FSharp.Data.dll"


// MAC
#r @"/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/packages/FSharp.Data.2.3.2/lib/net40/FSharp.Data.dll"
#r @"/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/packages/FSharp.Data.2.3.2/lib/net40/FSharp.Data.dll"




// let printSourceLocation() = 
//     printfn "Line: %s" __SOURCE_DIRECTORY__
//     printfn "Source Directory: %s" __SOURCE_DIRECTORY__
//     printfn "Source File: %s" __SOURCE_FILE__
// printSourceLocation()

// right click on references: add references > projects > solutions


// converts each binary string to a integer 
let parseLine (line:string)= 
    int ("0b"+ line.Replace(" ",""))

// how to get the current directory in F# interactive
//     Environment.CurrentDirectory;

// DEEDLE
// MAC
#nowarn "211"
#r @"/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/packages/Deedle.1.2.5/lib/net40/Deedle.dll"

// for F# interactive only ???

// do fsi.AddPrinter(fun (printer:Deedle.Internal.IFsiFormattable) -> "\n" + (printer.Format()))
// open Deedle




#nowarn "211"
#nowarn "40"

// When compiling, a reference to this is needed

