// how to load packages in F# interactive //
#I @"C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algos\Algos\packages\FSharpx.Collections.1.14.0\lib\net40"
#r @"FSharpx.Collections.dll"
//don't forget to type in F# interactive

open FSharpx.Collections

// https://www.nuget.org/packages/FSharp.Core
// FSharp.Core for F# 4.0 4.0.0.1
// Install-Package FSharp.Core
#I @"C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algos\Algos\packages\Spreads.0.3.6\lib\net45"
#I @"C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algos\Algos\packages\Spreads.Core.0.3.6\lib\net45"
#I @"C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algos\Algos\packages\Spreads.Extensions.0.3.6\lib\net45"

#r @"Spreads.dll"
#r @"Spreads.Core.dll"
#r @"Spreads.Collections.dll"
#r @"Spreads.Extensions.dll"

open Spreads
open Spreads.Collections

#I @"C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algos\packages\MathNet.Numerics.FSharp.3.11.0\lib\portable-net45+sl5+netcore45+MonoAndroid1+MonoTouch1"
#r @"MathNet.Numerics.FSharp.dll"

open System.Numerics

// let printSourceLocation() = 
//     printfn "Line: %s" __SOURCE_DIRECTORY__
//     printfn "Source Directory: %s" __SOURCE_DIRECTORY__
//     printfn "Source File: %s" __SOURC_FILE__
// printSourceLocation()

// right click on references: add references > projects > solutions


// converts each binary string to a integer 
let parseLine (line:string)= 
    int ("0b"+ line.Replace(" ",""))

// References
// AssemblyInfo.fs
// .fsx
// .fs
// App.config
// packages.config