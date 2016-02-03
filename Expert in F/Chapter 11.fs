#I "C:\Users\Fagui\Documents\GitHub\Learning Fsharp\Expert in F"
#load @"packages\FSharp.Charting.0.90.13\FSharp.Charting.fsx"
open FSharp.Charting

let rnd = System.Random()
let rand() = rnd.NextDouble()
let randomPoints = [for i in 0 .. 1000 -> 10.0 * rand(), 10.0 * rand()]
randomPoints |> Chart.Point

let randomTrend1 = [for i in 0.0 .. 0.1 .. 10.0 -> i, sin i + rand()]
let randomTrend2 = [for i in 0.0 .. 0.1 .. 10.0 -> i, sin i + rand()]
Chart.Combine [Chart.Line randomTrend1; Chart.Point randomTrend2]

randomPoints
|> fun c -> Chart.Line (c,Title="Expected Trend")

let rnd = new System.Random()
let rand() = rnd.NextDouble()
let data = [for i in 1 .. 1000 -> rand() * rand()]

let averageOfData = data |> Seq.average
let sumOfData = data |> Seq.sum
let maxOfData = data |> Seq.max
let minOfData = data |> Seq.min

type RandomPoint = {X : float; Y : float; Z : float}
let random3Dpoints =
    [for i in 1 .. 1000 -> {X = rand(); Y = rand(); Z = rand()}]
let averageX = random3Dpoints |> Seq.averageBy (fun p -> p.X)
let averageY = random3Dpoints |> Seq.averageBy (fun p -> p.Y)
let averageZ = random3Dpoints |> Seq.averageBy (fun p -> p.Z)

let maxY = random3Dpoints |> Seq.maxBy (fun p -> p.Y)

let norm (p : RandomPoint) = sqrt (p.X * p.X + p.Y * p.Y + p.Z * p.Z)
let closest = random3Dpoints |> Seq.minBy (fun p -> norm p)

let histogram =
    random3Dpoints
    |> Seq.countBy (fun p -> int (norm p * 10.0 / sqrt 3.0) )
    |> Seq.sortBy fst
    |> Seq.toList

/// Compute the variance of an array of inputs
let variance (values : float[]) =
    let sqr x = x * x
    let avg = values |> Array.average
    let sigma2 = values |> Array.averageBy (fun x -> sqr (x - avg))
    sigma2

let standardDeviation values =
    sqrt (variance values)

module Seq =
     /// Compute the variance of the given statistic from from the input data
    let varianceBy (f : 'T -> float) values =
        let sqr x = x * x
        let xs = values |> Seq.map f |> Seq.toArray
        let avg = xs |> Array.average
        let res = xs |> Array.averageBy (fun x -> sqr (x - avg))
        res

     /// Compute the standard deviation of the given statistic drawn from the input data
    let standardDeviationBy f values =
        sqrt (varianceBy f values)

let inline variance values =
    let sqr x = x * x
    let avg = values |> Array.average
    let sigma2 = values |> Array.averageBy (fun x -> sqr (x - avg))
    sigma2

let inline standardDeviation values =
    sqrt (variance values)

/// Listing 10-1. KMeans Clustering Algorithm

type Input<'T> = {Data : 'T; Features : float[]}
type Centroid = float[]

module Array =
    /// Like Seq.groupBy, but returns arrays
    let classifyBy f (xs : _[]) =
         xs |> Seq.groupBy f |> Seq.map (fun (k, v) -> (k, Seq.toArray v)) |> Seq.toArray
module Seq =
    /// Return x, f(x), f(f(x)), f(f(f(x))), ...
    let iterate f x = x |> Seq.unfold (fun x -> Some (x, f x))

/// Compute the norm distance between an input and a centroid
let distance (xs : Input<_>) (ys : Centroid) =
    (xs.Features,ys)
        ||> Array.map2 (fun x y -> (x - y) * (x - y))
        |> Array.sum
/// Find the average of set of inputs. First compute xs1 + ... + xsN, pointwise,
/// then divide each element of the sum by the number of inputs.
let computeCentroidOfGroup (_, group : Input<_>[]) =
    let e0 = group.[0].Features
    [|for i in 0 .. e0.Length - 1 -> group |> Array.averageBy (fun e -> e.Features.[i])|]

/// Group all the inputs by the nearest centroid
let classifyIntoGroups inputs centroids =
    inputs |> Array.classifyBy (fun v -> centroids |> Array.minBy (distance v))

/// Repeatedly classify the inputs, starting with the initial centroids
let rec computeCentroids inputs centroids = seq {
    let classification = classifyIntoGroups inputs centroids
    yield classification
    let newCentroids = Array.map computeCentroidOfGroup classification
    yield! computeCentroids inputs newCentroids}

/// Extract the features and repeatedly classify the inputs, starting with the
/// initial centroids
let kmeans inputs featureExtractor initialCentroids =
    let inputs =
        inputs
        |> Seq.map (fun i -> {Data = i; Features = featureExtractor i})
        |> Seq.toArray
    let initialCentroids = initialCentroids |> Seq.toArray
    computeCentroids inputs initialCentroids

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
type Observation = {Time : float<s>; Location : float<m>}
let rnd = System.Random()
let rand() = rnd.NextDouble()
let randZ() = rnd.NextDouble() - 0.5
/// Create a point near the given point
let near p = {Time= p.Time + randZ() * 20.0<s>;
              Location = p.Location + randZ() * 5.0<m>}
let data =
    [for i in 1 .. 1000 -> near {Time= 100.0<s>; Location = 60.0<m>}
     for i in 1 .. 1000 -> near {Time= 120.0<s>; Location = 80.0<m>}
     for i in 1 .. 1000 -> near {Time= 180.0<s>; Location = 30.0<m>}
     for i in 1 .. 1000 -> near {Time= 70.0<s>; Location = 40.0<m>}]

let maxTime = data |> Seq.maxBy (fun p -> p.Time) |> fun p -> p.Time
let maxLoc = data |> Seq.maxBy (fun p -> p.Location) |> fun p -> p.Location
let initialCentroids = [for i in 0 .. 9 -> [|rand(); rand()|]]
let featureExtractor (p : Observation) = [|p.Time / maxTime; p.Location / maxLoc|]

kmeans data featureExtractor initialCentroids
   |> Seq.map (Array.map (fun (c, _) -> c.[0] * maxTime, c.[1] * maxLoc))
   |> Seq.nth 100