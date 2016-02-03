open System
open RDotNet
open RProvider
open RProvider.graphics
open RProvider.stats 

// let x = System.Environment.CurrentDirectory
// val x : string

// loading RProvider in F# interactive
// #I "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Algo Stanford";;
// #load "packages\RProvider.1.1.15\RProvider.fsx";;

printfn "hello world"

Console.ReadKey() |> ignore

// Random number generator
let rng = Random()
let rand () = rng.NextDouble()

// Generate fake X1 and X2 
let X1s = [ for i in 0 .. 9 -> 10. * rand () ]
let X2s = [ for i in 0 .. 9 -> 5. * rand () ]

// Build Ys, following the "true" model
let Ys = [ for i in 0 .. 9 -> 5. + 3. * X1s.[i] - 2. * X2s.[i] + rand () ]

let dataset =
    namedParams [
        "Y", box Ys;
        "X1", box X1s;
        "X2", box X2s; ]
    |> R.data_frame

let result = R.lm(formula = "Y~X1+X2", data = dataset)
let coefficients = result.AsList().["coefficients"].AsNumeric()
let residuals = result.AsList().["residuals"].AsNumeric()
let summary = R.summary(result)
summary.AsList().["r.squared"].AsNumeric() |>ignore
R.plot result |> ignore

///////////////

let widgets = [ 3; 8; 12; 15; 19; 18; 18; 20; ]
let sprockets = [ 5; 4; 6; 7; 12; 9; 5; 6; ]

R.plot(widgets) |> ignore

R.plot(widgets, sprockets) |> ignore

R.barplot(widgets) |> ignore

R.hist(sprockets) |> ignore

R.pie(widgets) |> ignore

// Required package to save charts
open RProvider.grDevices

// Create path to an image testimage.png on the Desktop
let desktop = Environment.GetFolderPath(Environment.SpecialFolder.Desktop)  
let path = desktop + @"\testimage.png"

// Open the device and create the file as a png.
// R.bmp, R.jpeg, R.pdf, ... will generate other formats.
R.png(filename=path, height=200, width=300, bg="white") |> ignore
// Create the chart into the file
R.barplot(widgets) |> ignore
// Close the device once the chart is complete
R.dev_off () |> ignore