#nowarn "211"
// Standard NuGet or Paket location
#I "."
#I "lib/net40"

// Standard NuGet locations for R.NET
#I "../R.NET.Community.1.6.4/lib/net40"
#I "../R.NET.Community.FSharp.1.6.4/lib/net40"

// Standard Paket locations for R.NET
#I "../R.NET.Community/lib/net40"
#I "../R.NET.Community.FSharp/lib/net40"

// Try various folders that people might like
#I "bin"
#I "../bin"
#I "../../bin"
#I "lib"

// added by me
#I @"C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\packages\RProvider.1.1.15\lib\net40"
#I @"C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\packages\R.NET.Community.1.6.4\lib\net40"
#I @"C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford\packages\R.NET.Community.FSharp.1.6.4\lib\net40"

// Reference RProvider and RDotNet 
#r "RDotNet.dll"
#r "RDotNet.FSharp.dll"
#r "RProvider.dll"
#r "RProvider.Runtime.dll"

open RProvider
do fsi.AddPrinter(fun (synexpr:RDotNet.SymbolicExpression) -> synexpr.Print())