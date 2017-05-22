open System
open Deedle
open FSharp.Data

// type Stocks = CsvProvider<"/Users/francois-guillaume.rideau/Documents/MSFT.csv">
// let fb = Stocks.Load("http://ichart.finance.yahoo.com/table.csv?s=FB")

let dates  = 
  [ DateTime(2013,1,1); 
    DateTime(2013,1,4); 
    DateTime(2013,1,8) ]
let values = 
  [ 10.0; 20.0; 30.0 ]
let first = Series(dates, values)

// Create from a single list of observations
Series.ofObservations
  [ DateTime(2013,1,1) => 10.0
    DateTime(2013,1,4) => 20.0
    DateTime(2013,1,8) => 30.0 ]

series [ 1 => 1.0; 2 => 2.0 ];;
Series.ofValues [ 10.0; 20.0; 30.0 ]

/// Generate date range from 'first' with 'count' days
let dateRange (first:System.DateTime) count = seq {for i in 0..(count-1) ->  (first.AddDays(float i))}

/// Generate 'count' number of random doubles
let rand count = let rnd = System.Random()
                 seq {for i in 0..(count-1) -> rnd.NextDouble()}

// A series with values for 10 days 
let second = Series(dateRange (DateTime(2013,1,1)) 10, rand 10)

let df1 = Frame(["first"; "second"], [first; second])

// The same as previously
let df2 = Frame.ofColumns ["first" => first; "second" => second]

// Transposed - here, rows are "first" and "second" & columns are dates
let df3 = Frame.ofRows ["first" => first; "second" => second]

// Create from individual observations (row * column * value)
let df4 = 
  [ ("Monday", "Tomas", 1.0); ("Tuesday", "Adam", 2.1)
    ("Tuesday", "Tomas", 4.0); ("Wednesday", "Tomas", -5.4) ]
  |> Frame.ofValues

// Assuming we have a record 'Price' and a collection 'values'
type Price = { Day : DateTime; Open : float }
let prices = 
  [ { Day = DateTime.Now; Open = 10.1 }
    { Day = DateTime.Now.AddDays(1.0); Open = 15.1 }
    { Day = DateTime.Now.AddDays(2.0); Open = 9.1 } ]

// Creates a data frame with columns 'Day' and 'Open'
let df5 = Frame.ofRecords prices

//////////////////////////

let msftCsv = Frame.ReadCsv("/Users/francois-guillaume.rideau/Documents/MSFT.csv")
let fbCsv = Frame.ReadCsv("/Users/francois-guillaume.rideau/Documents/FB.csv")

// Use the Date column as the index & order rows
let msftOrd = 
  msftCsv
  |> Frame.indexRowsDate "Date"
  |> Frame.sortRowsByKey

// Create data frame with just Open and Close prices
let msft = msftOrd.Columns.[ ["Open"; "Close"] ]

// Add new column with the difference between Open & Close
msft?Difference <- msft?Open - msft?Close

// Do the same thing for Facebook
let fb = 
  fbCsv
  |> Frame.indexRowsDate "Date"
  |> Frame.sortRowsByKey
  |> Frame.sliceCols ["Open"; "Close"]
fb?Difference <- fb?Open - fb?Close

//// Now we can easily plot the differences
//Chart.Combine
//  [ Chart.Line(msft?Difference |> Series.observations) 
//    Chart.Line(fb?Difference |> Series.observations) ]

// Change the column names so that they are unique
let msftNames = ["MsftOpen"; "MsftClose"; "MsftDiff"]
let msftRen = msft |> Frame.indexColsWith msftNames

let fbNames = ["FbOpen"; "FbClose"; "FbDiff"]
let fbRen = fb |> Frame.indexColsWith fbNames

// Outer join (align & fill with missing values)
let joinedOut = msftRen.Join(fbRen, kind=JoinKind.Outer)

// Inner join (remove rows with missing values)
let joinedIn = msftRen.Join(fbRen, kind=JoinKind.Inner)

// Visualize daily differences on available values only
Chart.Rows
  [ Chart.Line(joinedIn?MsftDiff |> Series.observations) 
    Chart.Line(joinedIn?FbDiff |> Series.observations) ]

// Look for a row at a specific date
joinedIn.Rows.[DateTime(2013, 1, 2)]

GetAs<int>("FbOpen")
//val it : ObjectSeries<string> =
//  FbOpen    -> 28.00            
//  FbClose   -> 27.44   
//  FbDiff    -> -0.5599 
//  MsftOpen  -> 27.62   
//  MsftClose -> 27.25    
//  MsftDiff  -> -0.3700 

// Get opening Facebook price for 2 Jan 2013
joinedIn.Rows.[DateTime(2013, 1, 2)]?FbOpen
//val it : float = 28.0
joinedIn.Rows.[DateTime(2013, 1, 2)].GetAs<int>("FbOpen")
joinedIn.Rows.[DateTime(2013, 1, 2)].TryGetAs<int>("FbOpen")

// Get values for the first three days of January 2013
let janDates = [ for d in 2 .. 4 -> DateTime(2013, 1, d) ]
let jan234 = joinedIn.Rows.[janDates]

// Calculate mean of Open price for 3 days
jan234?MsftOpen |> Stats.mean

// Get values corresponding to entire January 2013
let jan = joinedIn.Rows.[DateTime(2013, 1, 1) .. DateTime(2013, 1, 31)] 

// Calculate means over the period
jan?FbOpen |> Stats.mean
jan?MsftOpen |> Stats.mean


////////////


let daysSeries = Series(dateRange DateTime.Today 10, rand 10)
let obsSeries = Series(dateRange DateTime.Now 10, rand 10)

// Fails, because current time is not present
try daysSeries.[DateTime.Now] with _ -> nan
try obsSeries.[DateTime.Now] with _ -> nan

// This works - we get the value for DateTime.Today (12:00 AM)
daysSeries.Get(DateTime.Now, Lookup.ExactOrSmaller)
// This does not - there is no nearest key <= Today 12:00 AM
try obsSeries.Get(DateTime.Today, Lookup.ExactOrSmaller)
with _ -> nan


let daysFrame = [ 1 => daysSeries ] |> Frame.ofColumns
let obsFrame = [ 2 => obsSeries ] |> Frame.ofColumns

// All values in column 2 are missing (because the times do not match)
let obsDaysExact = daysFrame.Join(obsFrame, kind=JoinKind.Left)

// All values are available - for each day, we find the nearest smaller
// time in the frame indexed by later times in the day

// le contraire !!!!

let obsDaysPrev = 
  (daysFrame, obsFrame) 
  ||> Frame.joinAlign JoinKind.Left Lookup.ExactOrSmaller

// The first value is missing (because there is no nearest 
// value with greater key - the first one has the smallest 
// key) but the rest is available

// le contraire !!!!
let obsDaysNext =
  (daysFrame, obsFrame) 
  ||> Frame.joinAlign JoinKind.Left Lookup.ExactOrGreater

// Projection and filtering
joinedOut.Columns |> Series.hasAll ["MsftOpen";"FbOpen"]

joinedOut?Comparison <- joinedOut |> Frame.mapRowValues (fun row -> 
  if row?MsftOpen > row?FbOpen then "MSFT" else "FB")

joinedOut.GetColumn<string>("Comparison")
  |> Series.filterValues ((=) "MSFT") |> Series.countValues

joinedOut.GetColumn<string>("Comparison")
  |> Series.filterValues ((=) "FB") |> Series.countValues

// Get data frame with only 'Open' columns
let joinedOpens = joinedOut.Columns.[ ["MsftOpen"; "FbOpen"] ]

// Get only rows that don't have any missing values
// and then we can safely filter & count
joinedOpens.RowsDense
   |> Series.filterValues (fun row -> row?MsftOpen > row?FbOpen)
   |> Series.countValues

/// Grouping and aggregation

let monthly =
  joinedIn
  |> Frame.groupRowsUsing (fun k _ -> DateTime(k.Year, k.Month, 1))

monthly.Rows.[DateTime(2013,5,1), *] |> Stats.mean

monthly 
  |> Frame.getNumericCols
  |> Series.mapValues (Stats.levelMean fst) // trick !!!! remember this one !!!
  |> Frame.ofColumns

/// Working with series and time series data in F#

// Use Math.NET for probability distributions
#r "MathNet.Numerics.dll"
open MathNet.Numerics.Distributions

/// Generates price using geometric Brownian motion
///  - 'seed' specifies the seed for random number generator
///  - 'drift' and 'volatility' set properties of the price movement
///  - 'initial' and 'start' specify the initial price and date
///  - 'span' specifies time span between individual observations
///  - 'count' is the number of required values to generate
let randomPrice seed drift volatility initial start span count = 
  (*[omit:(Implementation omitted)]*) 
  let dist = Normal(0.0, 1.0, RandomSource=Random(seed))  
  let dt = (span:TimeSpan).TotalDays / 250.0
  let driftExp = (drift - 0.5 * pown volatility 2) * dt
  let randExp = volatility * (sqrt dt)
  ((start:DateTimeOffset), initial) |> Seq.unfold (fun (dt, price) ->
    let price = price * exp (driftExp + randExp * dist.Sample()) 
    Some((dt, price), (dt + span, price))) |> Seq.take count(*[/omit]*)

// 12:00 AM today, in current time zone
let today = DateTimeOffset(DateTime.Today)
let stock1 = randomPrice 1 0.1 3.0 20.0 today 
let stock2 = randomPrice 2 0.2 1.5 22.0 today

//Chart.Line
//  [ stock1 (TimeSpan(0, 1, 0)) 1000 ;
//    stock2 (TimeSpan(0, 1, 0)) 1000  ]

let s1 =  (stock1 (TimeSpan(0, 1, 0)) 1000)
let s2 =  (stock2 (TimeSpan(0, 1, 0)) 1000)

let myfun s =
    let keys = Seq.map fst s
    let values = Seq.map snd s
    (Series.ofValues values) |> (Series.indexWith keys)

let ss1 = myfun s1
let ss2 = myfun s2
let df = Frame(["stock1";"stock2"],[ss1;ss2])

// Create input series with 6 observations
let lf = stock1 (TimeSpan(0, 1, 0)) 6 |> series

// Create series of series representing individual windows
// attention à la syntaxe !!!

lf |> Series.window 4
// Aggregate each window using 'Stats.mean'
lf |> Series.windowInto 4 Stats.mean
// Get first value in each window
lf |> Series.windowInto 4 Series.firstValue

// Calculate means for sliding windows
let lfm1 = lf |> Series.windowInto 4 Stats.mean
// Construct dataframe to show aligned results
Frame.ofColumns [ "Orig" => lf; "Means" => lfm1 ]

let lfm2 = 
  // Create sliding windows with incomplete windows at the beginning
  lf |> Series.windowSizeInto (4, Boundary.AtBeginning) (fun ds ->
    Stats.mean ds.Data)

Frame.ofColumns [ "Orig" => lf; "Means" => lfm2 ]

let lfm3 = 
  // Create sliding windows with incomplete windows at the ending
  lf |> Series.windowSizeInto (4, Boundary.AtEnding) (fun ds ->
    Stats.mean ds.Data)

Frame.ofColumns [ "Orig" => lf; "Means" => lfm3 ]

// Simple series with characters
let st = Series.ofValues [ 'a' .. 'e' ]
st |> Series.windowSizeInto (3, Boundary.AtEnding) (function
  | DataSegment.Complete(ser) -> 
      // Return complete windows as uppercase strings
      String(ser |> Series.values |> Array.ofSeq).ToUpper()
  | DataSegment.Incomplete(ser) -> 
      // Return incomplete windows as padded lowercase strings
      String(ser |> Series.values |> Array.ofSeq).PadRight(3, '-') )

// Window size conditions
// Generate prices for each hour over 30 days
let hourly = stock1 (TimeSpan(1, 0, 0)) (30*24) |> series

// Generate windows of size 1 day (if the source was
// irregular, windows would have varying size)
hourly |> Series.windowDist (TimeSpan(24, 0, 0))

// Generate windows such that date in each window is the same
// (windows start every hour and end at the end of the day)
hourly |> Series.windowWhile (fun d1 d2 -> d1.Date = d2.Date)

// Generate per-second observations over 10 minutes
let hf = stock1 (TimeSpan(0, 0, 1)) 600 |> series

// Create 10 second chunks with (possible) incomplete
// chunk of smaller size at the end.
hf |> Series.chunkSize (10, Boundary.AtEnding) 

// Create 10 second chunks using time span and get
// the first observation for each chunk (downsample)
hf |> Series.chunkDistInto (TimeSpan(0, 0, 10)) Series.firstValue

// Create chunks where hh:mm component is the same
// (containing observations for all seconds in the minute)
hf |> Series.chunkWhile (fun k1 k2 -> 
  (k1.Hour, k1.Minute) = (k2.Hour, k2.Minute))

// Create a series of pairs from earlier 'hf' input
hf |> Series.pairwise 

// Calculate differences between the current and previous values
hf |> Series.pairwiseWith (fun k (v1, v2) -> v2 - v1)

////

// Generate a bit less than 24 hours of data with 13.7sec offsets
let mf = stock1 (TimeSpan.FromSeconds(13.7)) 6300 |> series
// Generate keys for all minutes in 24 hours
let keys = [ for m in 0.0 .. 24.0*60.0-1.0 -> today.AddMinutes(m) ]

// Find value for a given key, or nearest greater key with value
mf |> Series.lookupAll keys Lookup.ExactOrGreater
       

// Find value for nearest smaller key
// (This returns value for 11:59:00 PM as well)
mf |> Series.lookupAll keys Lookup.ExactOrSmaller

// Find values for exact key 
// (This only works for the first key)
mf |> Series.lookupAll keys Lookup.Exact

// For each key, collect values for greater keys until the 
// next one (chunk for 11:59:00 PM is empty)
mf |> Series.resample keys Direction.Forward

// For each key, collect values for smaller keys until the 
// previous one (the first chunk will be singleton series)
mf |> Series.resample keys Direction.Backward

// Aggregate each chunk of preceding values using mean
mf |> Series.resampleInto keys Direction.Backward 
  (fun k s -> Stats.mean s)

// Resampling is also available via the member syntax
mf.Resample(keys, Direction.Forward)

// Generate 2.5 months of data in 1.7 hour offsets
let ds = stock1 (TimeSpan.FromHours(1.7)) 1000 |> series

// Sample by day (of type 'DateTime')
ds |> Series.resampleEquiv (fun d -> d.Date)

// Sample by day (of type 'DateTime')
ds.ResampleEquivalence(fun d -> d.Date)




Chart.Line df

Chart.Line [ for x in 0. .. 0.5 .. 6.3 -> x, sin x ]
   |> Chart.WithOptions(Options(curveType = "function"))

let sales = [("2013", 1000); ("2014", 1170); ("2015", 660); ("2016", 1030)]
let expenses = [("2013", 400); ("2014", 460); ("2015", 1120); ("2016", 540)]
 
let options =
  Options
    ( title = "Company Performance", curveType = "function",
      legend = Legend(position = "bottom") )
            
[sales; expenses]
   |> Chart.Line
   |> Chart.WithOptions options
   |> Chart.WithLabels ["Sales"; "Expenses"]



// Math.NET
open MathNet.Numerics.LinearAlgebra
let m = matrix [[ 1.0; 2.0 ]
                [ 3.0; 4.0 ]]
let m' = m.Inverse()

type Person = 
  { Name:string; Age:int; Countries:string list; }

let peopleRecds = 
  [ { Name = "Joe"; Age = 51; Countries = [ "UK"; "US"; "UK"] }
    { Name = "Tomas"; Age = 28; Countries = [ "CZ"; "UK"; "US"; "CZ" ] }
    { Name = "Eve"; Age = 2; Countries = [ "FR" ] }
    { Name = "Suzanne"; Age = 15; Countries = [ "US" ] } ]

// Turn the list of records into data frame 
let peopleList = Frame.ofRecords peopleRecds
// Use the 'Name' column as a key (of type string)
let people = peopleList |> Frame.indexRowsString "Name"

// Create frame with single column 'People'
let peopleNested = 
  [ "People" => Series.ofValues peopleRecds ] |> frame

// Expand the 'People' column
peopleNested |> Frame.expandCols ["People"]


let tuples = 
  [ dict ["A", box 1; "C", box (2, 3)]
    dict ["B", box 1; "C", box (3, 4)] ] 
  |> Series.ofValues

// Expand dictionary keys (level 1) and tuple items (level 2)
frame ["Tuples" => tuples]
|> Frame.expandAllCols 2

// Get the 'Age' column as a series of 'float' values
// (the '?' operator converts values automatically)
people?Age
// Get the 'Countries' column as a series of 'string list' values
people.GetColumn<string list>("Countries")
// Get all frame columns as a series of series
people.Columns

// Get Series<string, float> 
let numAges = people?Age

// Get value using question mark
numAges?Tomas
// Get value using 'Get' method
numAges.Get("Tomas")
// Returns missing when key is not found
numAges.TryGet("Fridrich")

people.Columns?Age
people.Columns?Age.TryAs<string>()open 
 //Stopped due to error
 //System.Exception: Operation could not be completed due to earlier error
 //Lookup on object of indeterminate type based on information prior to this program point. A type annotation may be 

// this is ok

// Iterate over rows and get the length of country list
people.Rows |> Series.mapValues (fun row ->
  row.GetAs<string list>("Countries").Length)


/// Expected columns & their types in a row
type IPerson = 
  abstract Age : int
  abstract Countries : string list

// Get rows as series of 'IPerson' values
let rows = people.GetRowsAs<IPerson>()
rows.["Tomas"].Countries 

// Create series with more value
let more = series [ "John" => 48.0 ]
// Create a new, concatenated series
people?Age.Merge(more)


// Calculate age + 1 for all people
let add1 = people?Age |> Series.mapValues ((+) 1.0)

// Add as a new series to the frame
people?AgePlusOne <- add1

// Add new series from a list of values
people?Siblings <- [0; 2; 1; 3]

// Replace existing series with new values
// (Equivalent to people?Siblings <- ...)
people.ReplaceColumn("Siblings", [3; 2; 1; 0])

// Create new object series with values for required columns
let newRow = 
  [ "Name" => box "Jim"; "Age" => box 51;
    "Countries" => box ["US"]; "Siblings" => box 5 ]
  |> series
// Create a new data frame, containing the new series
people.Merge("Jim", newRow)

// Another option is to use mutable SeriesBuilder
let otherRow = SeriesBuilder<string>()
otherRow?Name <- "Jim"
otherRow?Age <- 51
otherRow?Countries <- ["US"]
otherRow?Siblings <- 5
// The Series property returns the built series
people.Merge("Jim", otherRow.Series)

// Advanced slicing and lookup
// Sample series with different keys & values
let nums = series [ 1 => 10.0; 2 => 20.0 ]
let strs = series [ "en" => "Hi"; "cz" => "Ahoj" ]

// Lookup values using keys
nums.[1]
strs.["en"]
// Supported when key is string
strs?en    

// Get an unordered sample series 
let ages = people?Age

// Returns value for a given key
ages.["Tomas"]
// Returns series with two keys from the source
ages.[ ["Tomas"; "Joe"] ]

// Fails when key is not present
try ages |> Series.get "John" with _ -> nan
// Returns 'None' when key is not present
ages |> Series.tryGet "John"
// Returns series with missing value for 'John'
// (equivalent to 'ages.[ ["Tomas"; "John"] ]')
ages |> Series.getAll [ "Tomas"; "John" ]


// Get all observations as a sequence of 'KeyValuePair'
ages.Observations
// Get all observations as a sequence of tuples
ages |> Series.observations
// Get all observations, with 'None' for missing values
ages |> Series.observationsAll


let travels = people.GetColumn<string list>("Countries")

// Group by name length (ignoring visited countries)
travels |> Series.groupBy (fun k v -> k.Length)
// Group by visited countries (people visited/not visited US)
travels |> Series.groupBy (fun k v -> List.exists ((=) "US") v)

// Group by name length and get number of values in each group
travels |> Series.groupInto 
  (fun k v -> k.Length) 
  (fun len people -> Series.countKeys people)

travels
|> Series.mapValues (Seq.countBy id >> series)
|> Frame.ofRows
|> Frame.fillMissingWith 0

[ Nullable(1); Nullable(); Nullable(3) ]
|> Series.ofValues

type AirQuality = CsvProvider<"/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/The42project/AirQuality.csv", ";">
let airquality = AirQuality.GetSample()

let air = Frame.ReadCsv("/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/The42project/AirQuality.csv")
// ça a l'air plus pourri
// let air = Frame.ReadCsv("/Users/francois-guillaume.rideau/Documents/Learning-Fsharp/The_Fsharp_Solution_to_all_problems/The42project/AirQuality.csv",separators=";")
let ozone = air?Ozone

series [
  "Mean" => round (Stats.mean ozone)
  "Max" => Option.get (Stats.max ozone)
  "Min" => Option.get (Stats.min ozone)
  "Median" => Stats.median ozone ]

let info = 
  [ "Min" => Stats.min air
    "Max" => Stats.max air
    "Mean" => Stats.mean air
    "+/-" => Stats.stdDev air ] |> frame

ozone
|> Stats.movingMin 3

ozone
|> Stats.movingMean 3

open System.Globalization
let dateFormat = CultureInfo.CurrentCulture.DateTimeFormat
let byMonth = air |> Frame.indexRowsUsing (fun r ->
    dateFormat.GetMonthName(r.GetAs("Month")), r.GetAs<int>("Day"))

byMonth?Ozone
|> Stats.levelMean fst