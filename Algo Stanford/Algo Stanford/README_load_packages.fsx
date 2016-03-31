// how to load packages in F# interactive //

#r @"C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\packages\FSharpx.Collections.1.13.4\lib\net40\FSharpx.Collections.dll"
//don't forget to type in F# interactive

open FSharpx.Collections

// https://www.nuget.org/packages/FSharp.Core
// FSharp.Core for F# 4.0 4.0.0.1
// Install-Package FSharp.Core

#r @"C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\packages\Spreads.0.3.5\lib\net45\Spreads.dll"
#r @"C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\packages\Spreads.Core.0.3.5\lib\net45\Spreads.Core.dll"
#r @"C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\packages\Spreads.Core.0.3.5\lib\net45\Spreads.Collections.dll"
#r @"C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\packages\Spreads.Extensions.0.3.5\lib\net45\Spreads.Extensions.dll"

// let printSourceLocation() = 
//     printfn "Line: %s" __SOURCE_DIRECTORY__
//     printfn "Source Directory: %s" __SOURCE_DIRECTORY__
//     printfn "Source File: %s" __SOURC_FILE__
// printSourceLocation()

// right click on references: add references > projects > solutions


// converts each binary string to a integer 
let parseLine (line:string)= 
    int ("0b"+ line.Replace(" ",""))
