open System

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
/// Split a string into words at spaces

////// Chapter 2

let splitAtSpaces (text: string) =
    text.Split ' '
    |> Array.toList
/// Analyze a string for duplicate words
let wordCount text =
    let words = splitAtSpaces text
    let wordSet = Set.ofList words
    let numWords = words.Length
    let numDups = words.Length - wordSet.Count
    (numWords, numDups)
/// Analyze a string for duplicate words and display the results.
let showWordCount text =
    let numWords, numDups = wordCount text
    printfn "--> %d words in the text" numWords
    printfn "--> %d duplicate words" numDups

//let showResults (numWords, numDups) =
//    printfn "--> %d words in the text" numWords
//    printfn "--> %d duplicate words" numDups
//let showWordCount text = showResults (wordCount text)

let powerOfFour n =
    let nSquared = n * n in nSquared * nSquared
let powerOfFourPlusTwo n =
    let n = n * n
    let n = n * n
    let n = n + 2
    n
let powerOfFourPlusTwoTimesSix n =
    let n3 =
        let n1 = n * n
        let n2 = n1 * n1
        n2 + 2
    let n4 = n3 * 6
    n4

Set.ofList ["b"; "a"; "b"; "b"; "c" ];;
Set.toList (Set.ofList ["abc"; "ABC"]);;

let site1 = ("www.cnn.com", 10)
let site2 = ("news.bbc.com", 5)
let site3 = ("www.msnbc.com", 4)
let sites = (site1, site2, site3)

fst site1
// let relevance = snd site1

let url, relevance = site1
let siteA, siteB, siteC = sites

// System.Console.WriteLine("--> {0} words in the text", box numWords)
// System.Console.WriteLine("--> {0} duplicate words", box numDups)

let two = (printfn "Hello World"; 1 + 1)
let four = two + two

open System.Windows.Forms
let form = new Form(Visible = true, TopMost = true, Text = "Welcome to F#")
let textB = new RichTextBox(Dock = DockStyle.Fill, Text = "Here is some initial text")
form.Controls.Add textB

//open System.Windows.Forms
//let form = new Form()
//form.Visible <- true
//form.TopMost <- true
//form.Text <- “Welcome to F#”

open System.IO
open System.Net
/// Get the contents of the URL via a web request
let http (url: string) =
   let req = System.Net.WebRequest.Create(url)
   let resp = req.GetResponse()
   let stream = resp.GetResponseStream()
   let reader = new StreamReader(stream)
   let html = reader.ReadToEnd()
   resp.Close()
   html

textB.Text <- http "http://news.bbc.co.uk"

////// Chapter 3

let squareAndAdd a b = a * a + b
let s = "Couldn't put Humpty"
s.Length
s.[13]
s.[13..16]

let round x =
    match x with
    | _ when x >= 100 -> 100
    | _ when x < 0 -> 0
    | _ -> x

let round2 (x, y) =
    if x >= 100 || y >= 100 then 100, 100
    elif x < 0 || y < 0 then 0, 0
    else x, y

let rec length l =
    match l with
    | [] -> 0
    | h :: t -> 1 + length t

let rec repeatFetch url n =
    if n > 0 then
    let html = http url
    printfn "fetched <<< %s >>> on iteration %d" html n
    repeatFetch url (n - 1)

// définir 2 fonctions récursives imbriquées
let rec even n = (n = 0u) || odd(n - 1u)
and odd n = (n <> 0u) && even(n - 1u)

//let even n = (n % 2u) = 0u
//let odd n = (n % 2u) = 1u

let oddPrimes = [3; 5; 7; 11]
let morePrimes = [13; 17]
let primes = 2 :: (oddPrimes @ morePrimes)
//let people = [ "Adam"; "Dominic"; "James" ];;
//"Chris" :: people
//people

//List.head [5; 4; 3];;
//List.tail [5; 4; 3];;
//List.map (fun x -> x * x) [1; 2; 3];;
//List.filter (fun x -> x % 3 = 0) [2; 3; 5; 7; 9];;

let people = [("Adam", None);
              ("Eve" , None);
              ("Cain", Some("Adam","Eve"));
              ("Abel", Some("Adam","Eve"))];;

let fetch url =
    try Some (http url)
    with :? System.Net.WebException -> None

match (fetch "http://www.nature.com") with
| Some text -> printfn "text = %s" text
| None -> printfn "**** no web page found";;

let isLikelySecretAgent url agent =
    match (url, agent) with
    | "http://www.control.org", 99 -> true
    | "http://www.control.org", 86 -> true
    | "http://www.kaos.org", _ -> true | _ -> false

let printFirst primes =
    match primes with
    | h :: t -> printfn "The first prime in the list is %d" h
    | [] -> printfn "No primes found in the list"

let showParents (name, parents) =
    match parents with
    | Some (dad, mum) -> printfn "%s has father %s, mother %s" name dad mum
    | None -> printfn "%s has no parents!" name;;

showParents ("Adam", None);;
showParents ("me",Some ("Dad","Mum"))

let highLow a b =
    match (a, b) with
    | ("lo", lo), ("hi", hi) -> (lo, hi)
    | ("hi", hi), ("lo", lo) -> (lo, hi)
    | _ -> failwith "expected a both a high and low value"

highLow ("hi", 300) ("lo", 100);;

let sign x =
match x with
    | _ when x < 0 -> -1
    | _ when x > 0 -> 1
    | _ -> 0

let getValue a =
    match a with
    | (("lo" | "low"), v) -> v
    | ("hi", v) | ("high", v) -> v
    | _ -> failwith "expected a both a high and low value"

//Individual patterns can’t bind the same variables twice. For example, a pattern (x, x) isn’t permitted,
//although (x, y) when x = y is permitted. Furthermore, each side of an “or” pattern must bind the same set of
//variables, and these variables must have the same types.

let primeCubes = List.map (fun n -> n * n * n) primes
let resultsOfFetch = List.map (fun url -> (url, http url)) sites
List.map (fun (_,p) -> String.length p) resultsOfFetch

let delimiters = [| ' '; '\n'; '\t'; '<'; '>'; '=' |]
let getWords (s: string) = s.Split delimiters
let getStats site =
    let url = "http://" + site
    let html = http url
    let hwords = html |> getWords
    let hrefs = html |> getWords |> Array.filter (fun s -> s = "href")
    (site, html.Length, hwords.Length, hrefs.Length)
let sites = ["www.live.com"; "www.google.com"; "search.yahoo.com"]
sites |> List.map getStats;;

[1;2;3] |> List.map (fun x -> x * x * x)

let google = http "http://www.google.com"
google |> getWords |> Array.filter (fun s -> s = "href") |> Array.length
// same than 
let countLinks = getWords >> Array.filter (fun s -> s = "href") >> Array.length
google |> countLinks

let shift (dx, dy) (px, py) = (px + dx, py + dy)
let shiftRight = shift (1, 0)
let shiftUp = shift (0, 1)
let shiftLeft = shift (-1, 0)
let shiftDown = shift (0, -1)

open System.Drawing
let remap (r1:Rectangle) (r2:Rectangle) =
    let scalex = float r2.Width / float r1.Width
    let scaley = float r2.Height / float r1.Height
    let mapx x = int (float r2.Left + truncate (float (x - r1.Left) * scalex))
    let mapy y = int (float r2.Top + truncate (float (y - r1.Top) * scaley))
    let mapp (p:Point) = Point(mapx p.X, mapy p.Y)
    mapp

let mapp = remap (Rectangle(100, 100, 100, 100)) (Rectangle(50, 50, 200, 200));;
//The intermediate values scalex and scaley are computed only once, despite the fact that you’ve called
//the resulting function mapp three times. It may be helpful to think of mapp as a function object being
//generated by the remap function.

let sites = ["http://www.live.com";
             "http://www.google.com";
             "http://search.yahoo.com"]
sites |> List.iter (fun site -> printfn "%s, length = %d" site (http site).Length)

let time f =
    let start = DateTime.Now
    let res = f()
    let finish = DateTime.Now
    (res, finish - start)
time (fun () -> http "http://www.newscientist.com");;

open System.IO;;
[@"C:\Program Files"; @"C:\Windows"] |> List.map Directory.GetDirectories;;

let f = (Console.WriteLine : string -> unit);;

/////// Chapter 4

let repeatFetch url n =
    for i = 1 to n do
        let html = http url
        printf "fetched <<< %s >>>\n" html
    printf "Done!\n"

for (b, pj) in [("Banana 1", false); ("Banana 2", true)] do
    if pj then
        printfn "%s is in pyjamas today!" b;;

open System.Text.RegularExpressions;;
for m in Regex.Matches("All the Pretty Horses","[a-zA-Z]+") do
    printf "res = %s\n" m.Value;;

type DiscreteEventCounter =
    { mutable Total : int;
      mutable Positive : int;
      Name : string }

let recordEvent (s : DiscreteEventCounter) isPositive =
    s.Total <- s.Total+1
    if isPositive then s.Positive <- s.Positive + 1

let reportStatus (s : DiscreteEventCounter) =
    printfn "We have %d %s out of %d" s.Positive s.Name s.Total

let newCounter nm =
    { Total = 0;
      Positive = 0;
      Name = nm }

let longPageCounter = newCounter "long page(s)"

let fetch url =
    let page = http url
    recordEvent longPageCounter (page.Length > 10000)
    page

let generateStamp =
    let count = ref 0
    (fun () -> count := !count + 1; !count)
//It’s good programming practice in polished code to ensure that all related items of
//mutable state are collected under some named data structure or other entity, such as a function.

//Ultimately, you can detect whether two mutable values are the same object by using the function System.
//Object.ReferenceEquals

let sum n m =
    let mutable res = 0
    for i = n to m do
        res <- res + i
    res

//mutable arrays
let arr = [|1.0; 1.0; 1.0|];;
arr.[1];;
arr.[1] <- 3.0;;
arr;;
let arr = [|for i in 0 .. 5 -> (i, i * i)|];;
arr.[1..3];;
arr.[..2];;
arr.[3..];;

//Resizable arrays use an underlying array for storage and support constant-time random-access
//lookup.
let names = new ResizeArray<string>();;
for name in ["Claire"; "Sophie"; "Jane"] do
    names.Add(name);;
names.[2];;

let squares = new ResizeArray<int>(seq {for i in 0 .. 100 -> i * i});;
for x in squares do
    printfn "square: %d" x;;

// Dictionaries
open System.Collections.Generic;;
let capitals = new Dictionary<string, string>(HashIdentity.Structural);;
capitals.["USA"] <- "Washington";;
capitals.["Bangladesh"] <- "Dhaka";;
capitals.ContainsKey("USA");;
capitals.Keys;;
capitals.["USA"];;
for kvp in capitals do
    printf "%s has capital %s\n" kvp.Key kvp.Value;;
let lookupName nm (dict : Dictionary<string, string>) =
    let mutable res = ""
    let foundIt = dict.TryGetValue(nm, &res)
    if foundIt then res
    else failwithf "Didn't find %s" nm

// lookup dictionary with cells
let res = ref "";;
capitals.TryGetValue("Australia", res);;
capitals.TryGetValue("USA", res);;
res;;
// lookup dictionary with tuples
capitals.TryGetValue("Australia");;
capitals.TryGetValue("USA");;

// exceptions
if false then 3 else failwith "hit the wall";;

try
    raise (System.InvalidOperationException ("it's just not my day"))
with
    :? System.InvalidOperationException -> printfn "caught!"

try
    raise (new System.InvalidOperationException ("invalid operation"))
with
    err -> printfn "oops, msg = '%s'" err.Message

let httpViaTryFinally(url : string) =
    let req = System.Net.WebRequest.Create(url)
    let resp = req.GetResponse()
    try
       let stream = resp.GetResponseStream()
       let reader = new StreamReader(stream)
       let html = reader.ReadToEnd()
       html
    finally
        resp.Close()

// same than 
let httpViaUseBinding(url: string) =
    let req = System.Net.WebRequest.Create(url)
    use resp = req.GetResponse()
    let stream = resp.GetResponseStream()
    let reader = new StreamReader(stream)
    let html = reader.ReadToEnd()
    html
// create our own exception
exception BlockedURL of string

let http2 url =
    if url = "http://www.kaos.org"
    then raise(BlockedURL(url))
    else http url

try
    raise(BlockedURL("http://www.kaos.org"))
with
    BlockedURL(url) -> printfn "blocked! url = '%s'" url

/// basic IO

open System.IO;;
File.WriteAllLines("test.txt", [|"This is a test file.";
                                 "It is easy to read."|]);;
File.ReadAllLines "test.txt";;
File.ReadAllText "test.txt";;

seq { for line in File.ReadLines("test.txt") do
          let words = line.Split [|' '|]
          if words.Length > 3 && words.[2] = "easy" then
              yield line}
let outp = File.CreateText "playlist.txt";;
outp.WriteLine "Enchanted";;
outp.WriteLine "Put your records on";;
outp.Close();

let inp = File.OpenText("playlist.txt");;
inp.ReadLine();;
inp.ReadLine();;
inp.Close();;
 
/// Precomputation read page 67-68 very important
// this is good
let isWord (words : string list) =
    let wordTable = Set.ofList words
    fun w -> wordTable.Contains(w)
let isCapital = isWord ["London"; "Paris"; "Warsaw"; "Tokyo"];;
isCapital "Paris";;
isCapital "Manchester";;

// this is bad
let isCapitalSlow inp = isWord ["London"; "Paris"; "Warsaw"; "Tokyo"] inp

let isWordSlow2 (words : string list) (word : string) =
    List.exists (fun word2 -> word = word2) words
let isCapitalSlow2 word = isWordSlow2 ["London"; "Paris"; "Warsaw"; "Tokyo"] word
let isWordSlow3 (words : string list) (word : string) =
    let wordTable = Set<string>(words)
    wordTable.Contains(word)
let isCapitalSlow3 word = isWordSlow3 ["London"; "Paris"; "Warsaw"; "Tokyo"] word

// this is good too
let isWord (words : string list) =
    let wordTable = HashSet<string>(words)
    fun word -> wordTable.Contains word
// 
open System

type NameLookupService =
    abstract Contains : string -> bool

let buildSimpleNameLookup (words : string list) =
    let wordTable = HashSet<_>(words)
    {new NameLookupService with
         member t.Contains w = wordTable.Contains w}

let capitalLookup = buildSimpleNameLookup ["London"; "Paris"; "Warsaw"; "Tokyo"];;
capitalLookup.Contains "Paris";;

/// memoizing

let fibFast =
    let t = new System.Collections.Generic.Dictionary<int, int>()

    let rec fibCached n =
        if t.ContainsKey n then t.[n]
        elif n <= 2 then 1
        else let res = fibCached (n - 1) + fibCached (n - 2)
             t.Add (n, res)
             res
    fun n -> fibCached n

// From Chapter 2, but modified to use stop watch.
let time f =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let res = f()
    let finish = sw.Stop()
    (res, sw.Elapsed.TotalMilliseconds |> sprintf "%f ms")

time(fun () -> fibFast 30)
time(fun () -> fibFast 30)
time(fun () -> fibFast 30)

open System.Collections.Generic

let memoize (f : 'T -> 'U) =
    let t = new Dictionary<'T, 'U>(HashIdentity.Structural)
    fun n ->
        if t.ContainsKey n then t.[n]
        else let res = f n
             t.Add (n, res)
             res
let rec fibFast =
    memoize (fun n -> if n <= 2 then 1 else fibFast (n - 1) + fibFast (n - 2))
//attention
let rec fibNotFast n =
    memoize (fun n -> if n <= 2 then 1 else fibNotFast (n - 1) + fibNotFast (n - 2)) n

// a Generic memoization service

open System.Collections.Generic
type Table<'T, 'U> =
    abstract Item : 'T -> 'U with get
    abstract Discard : unit -> unit

let memoizeAndPermitDiscard f =
    let lookasideTable = new Dictionary<_, _>(HashIdentity.Structural)
    {new Table<'T, 'U> with
          member t.Item
             with get(n) =
                 if lookasideTable.ContainsKey(n)
                 then lookasideTable.[n]
                 else let res = f n
                      lookasideTable.Add(n, res)
                      res
          member t.Discard() =
              lookasideTable.Clear()}
#nowarn "40" // do not warn on recursive computed objects and functions

let rec fibFast =
memoizeAndPermitDiscard
    (fun n ->
        printfn "computing fibFast %d" n
        if n <= 2 then 1 else fibFast.[n - 1] + fibFast.[n - 2])

fibFast.[3];;
fibFast.[5];;
fibFast.Discard();;
fibFast.[5];;

/// lazy values
let sixty = lazy (30 + 30);;
sixty.Force();;
let sixtyWithSideEffect = lazy (printfn "Hello world"; 30 + 30);;
sixtyWithSideEffect.Force();;
sixtyWithSideEffect.Force();;

open System.Collections.Generic

let divideIntoEquivalenceClasses keyf seq =
    // The dictionary to hold the equivalence classes
    let dict = new Dictionary<'key, ResizeArray<'T>>()

    // Build the groupings
    seq |> Seq.iter (fun v ->
        let key = keyf v
        let ok, prev = dict.TryGetValue(key)
        if ok then prev.Add(v)
        else let prev = new ResizeArray<'T>()
             dict.[key] <- prev
             prev.Add(v))
    // Return the sequence-of-sequences. Don't reveal the
    // internal collections: just reveal them as sequences
    dict |> Seq.map (fun group -> group.Key, Seq.readonly group.Value)

divideIntoEquivalenceClasses (fun n -> n % 3) [0 .. 10];;

////// Chapter 5 //////

type Person =
    {Name : string
     DateOfBirth : System.DateTime}

type PageStats =
    {Site : string;
     Time : System.TimeSpan;
     Length : int;
     NumWords : int;
     NumHRefs : int}

//Using the time, http and getWords functions from Chapter 3.
let stats site =
    let url = "http://" + site
    let html, t = time (fun () -> http url)
    let words = html |> getWords
    let hrefs = words |> Array.filter (fun s -> s = "href")
    {Site = site; Time = t; Length = html.Length;
     NumWords = words.Length; NumHRefs = hrefs.Length}

type Person =
    {Name : string
     DateOfBirth : System.DateTime}
type Company =
    {Name : string
     Address : string}

type Dot = {X : int; Y : int}
type Point = {X : float; Y : float}
type Point3D = {X : float; Y : float; Z : float}

/// Discriminated Unions

type Route = int
type Make = string
type Model = string
type Transport =
    | Car of Make * Model
    | Bicycle
    | Bus of Route

type Proposition =
    | True
    | And of Proposition * Proposition
    | Or of Proposition * Proposition
    | Not of Proposition

let rec eval (p : Proposition) =
    match p with
    | True -> true
    | And(p1,p2) -> eval p1 && eval p2
    | Or (p1,p2) -> eval p1 || eval p2
    | Not(p1) -> not (eval p1)

type 'T option =
    | None
    | Some of 'T

type Tree<'T> =
    | Tree of 'T * Tree<'T> * Tree<'T>
    | Tip of 'T

let rec sizeOfTree tree =
    match tree with
    | Tree(_, l, r) -> 1 + sizeOfTree l + sizeOfTree r
    | Tip _ -> 1

type Point3D = Vector3D of float * float * float
let origin = Vector3D(0., 0., 0.)
let unitX = Vector3D(1., 0., 0.)
let unitY = Vector3D(0., 1., 0.)
let unitZ = Vector3D(0., 0., 1.)


/// this doesn't work p.84 ///
type Node =
    {Name : string;
     Links : Link list} and Link =
    | Dangling
    | Link of Node
//////
let rec map (f : 'T -> 'U) (l : 'T list) =
    match l with
    | h :: t -> f h :: map f t
    | [] -> []

let getFirst (a,b,c) = a;;
let mapPair f g (x, y) = (f x, g y);;

///Consider replacing uses of tuples as keys by
///the use of a new, named key type, often using a union type with a single case, e.g.
///type Key = Key of string * int. This allows the compiler to optimize the hashing.///

/// Generic Binary Serialization via .NET libraries

open System.Runtime.Serialization.Formatters.Binary
let writeValue outputStream (x : 'T) =
    let formatter = new BinaryFormatter()
    formatter.Serialize(outputStream, box x)
let readValue inputStream =
    let formatter = new BinaryFormatter()
    let res = formatter.Deserialize(inputStream)
    unbox res

open System.IO
let addresses = Map.ofList ["Jeff", "123 Main Street, Redmond, WA 98052"
                            "Fred", "987 Pine Road, Phila., PA 19116"
                            "Mary", "PO Box 112233, Palo Alto, CA 94301"]
let fsOut = new FileStream("Data.dat", FileMode.Create)
writeValue fsOut addresses
fsOut.Close()
let fsIn = new FileStream("Data.dat", FileMode.Open)
let res : Map<string, string> = readValue fsIn
fsIn.Close() 

///

let hcfGeneric (zero, sub, lessThan) =
    let rec hcf a b =
        if a = zero then b
        elif lessThan a b then hcf a (sub b a)
        else hcf (sub a b) b
    hcf

let hcfInt = hcfGeneric (0, (-), (<))
let hcfInt64 = hcfGeneric (0L, (-), (<))
let hcfBigInt = hcfGeneric (0I, (-), (<))

/// alternative way - simpler ?
type Numeric<'T> =
    {Zero : 'T;
     Subtract : ('T -> 'T -> 'T);
     LessThan : ('T -> 'T -> bool);}
let intOps = {Zero = 0; Subtract = (-); LessThan = (<)}
let bigintOps = {Zero = 0I; Subtract = (-); LessThan = (<)}
let int64Ops = {Zero = 0L; Subtract = (-); LessThan = (<)}

let hcfGeneric (ops : Numeric<'T>) =
     let rec hcf a b =
         if a = ops.Zero then b
         elif ops.LessThan a b then hcf a (ops.Subtract b a)
         else hcf (ops.Subtract a b) b
     hcf

let hcfInt = hcfGeneric intOps
let hcfBigInt = hcfGeneric bigintOps
/// another variant

type INumeric<'T> =
    abstract Zero : 'T
    abstract Subtract : 'T * 'T -> 'T
    abstract LessThan : 'T * 'T -> bool

let intOps =
    {new INumeric<int> with
        member ops.Zero = 0
        member ops.Subtract(x, y) = x - y
        member ops.LessThan(x, y) = x < y}

let hcfGeneric (ops : INumeric<'T>) =
    let rec hcf a b =
        if a = ops.Zero then b
        elif ops.LessThan(a, b) then hcf a (ops.Subtract(b, a))
        else hcf (ops.Subtract(a, b)) b
    hcf

let inline hcf a b =
    hcfGeneric
        {new INumeric<'T> with
             member ops.Zero = LanguagePrimitives.GenericZero<'T>
             member ops.Subtract(x, y) = x - y
             member ops.LessThan(x, y) = x < y}
         a b

let xobj = (1 :> obj);;
let sobj = ("abc" :> obj);;
let boxedObject = box "abc";;
let downcastString = (boxedObject :?> string);;

/// type test with pattern matching

let checkObject (x : obj) =
    match x with
    | :? string -> printfn "The object is a string"
    | :? int -> printfn "The object is an integer"
    | _ -> printfn "The input is something else"
let reportObject (x : obj) =
    match x with
    | :? string as s -> printfn "The input is the string '%s'" s
    | :? int as d -> printfn "The input is the integer '%d'" d
    | _ -> printfn "the input is something else"
reportObject (box 17);;

/// knowing when upcasts are applied automatically
/// what is this doing ?
open System.Windows.Forms;;
let setTextOfControl (c : Control) (s : string) = c.Text <- s;;
let form = new Form();;
let textBox = new TextBox();;
setTextOfControl form "Form Text";;
setTextOfControl textBox "Text Box Text";;
let form = new Form();;
let textBox = new TextBox();;
setTextOfControl form "Form Text";;
setTextOfControl textBox "Text Box Text";;
/// Add a type constraint to an argument, such as 
///let setTextOfControl (c : Control) (s : string) = c.Text <- s.

open System
open System.IO

let textReader =
    if DateTime.Today.DayOfWeek = DayOfWeek.Monday
    then Console.In
    else (File.OpenText("input.txt") :> TextReader) ///sans cet upcast il y a une erreur

/// upcasts aren’t applied automatically for the result of a function
let getTextReader () : TextReader = (File.OpenText("input.txt") :> TextReader)
/// make arguments explicit
let mapFirst inp = inp |> List.map (fun (x, y) -> x)
let printFstElements inp = inp |> List.map fst |> List.iter (printf "res = %d")
/// create empty list of 100 elements !
let empties () = Array.create 100 []
let intEmpties : int list [] = empties()
let stringEmpties : string list [] = empties()

///
let emptyLists<'T> : seq<'T list> = Seq.init 100 (fun _ -> [])
Seq.length emptyLists;;
emptyLists<int>;;
emptyLists<string>;;

////// Chapter 6 Object Programming

/// Listing 6-1

type Vector2D =
    { DX : float; DY : float }
    
    /// Get the length of the vector
    member v.Length = sqrt(v.DX * v.DX + v.DY * v.DY)
    /// Get a vector scaled by the given factor
    member v.Scale(k) = { DX = k * v.DX; DY = k * v.DY }
    /// Return a vector shifted by the given delta in the X coordinate
    member v.ShiftX(x) = { v with DX = v.DX + x }
    /// Return a vector shifted by the given delta in the Y coordinate
    member v.ShiftY(y) = { v with DY = v.DY + y }
    /// Return a vector shifted by the given distance in both coordinates
    member v.ShiftXY(x,y) = { DX = v.DX + x; DY = v.DY + y }
    /// Get the zero vector
    static member Zero = { DX = 0.0; DY = 0.0 }
    /// Return a constant vector along the X axis
    static member ConstX(dx) = { DX = dx; DY = 0.0 }
    /// Return a constant vector along the Y axis
    static member ConstY(dy) = { DX = 0.0; DY = dy }
    member v.LengthWithSideEffect =
        printfn "Computing!"
        sqrt(v.DX * v.DX + v.DY * v.DY)

/// Listing 6-2

type Vector2D(dx : float, dy : float) =
    let len = sqrt(dx * dx + dy * dy)
    /// Get the X component of the vector
    member v.DX = dx
    /// Get the Y component of the vector
    member v.DY = dy
    /// Get the length of the vector
    member v.Length = len
    /// Return a vector scaled by the given factor
    member v.Scale(k) = Vector2D(k * dx, k * dy)
    /// Return a vector shifted by the given delta in the Y coordinate
    member v.ShiftX(x) = Vector2D(dx = dx + x, dy = dy)
    /// Return a vector shifted by the given delta in the Y coordinate
    member v.ShiftY(y) = Vector2D(dx = dx, dy = dy + y)
    /// Return a vector that is shifted by the given deltas in each coordinate
    member v.ShiftXY(x, y) = Vector2D(dx = dx + x, dy = dy + y)
    /// Get the zero vector
    static member Zero = Vector2D(dx = 0.0, dy = 0.0)
    /// Get a constant vector along the X axis of length one
    static member OneX = Vector2D(dx = 1.0, dy = 0.0)
    /// Get a constant vector along the Y axis of length one
    static member OneY = Vector2D(dx = 0.0, dy = 1.0)

type UnitVector2D(dx,dy) =
    let tolerance = 0.000001
    let length = sqrt (dx * dx + dy * dy)
    do if abs (length - 1.0) >= tolerance then failwith "not a unit vector";
    member v.DX = dx
    member v.DY = dy
    new() = UnitVector2D (1.0,0.0)

let x = new UnitVector2D ();;

/// A class including some static bindings /// i don't get it !!!!!!!!
type Vector2D(dx : float, dy : float) =
     static let zero = Vector2D(0.0, 0.0)
     static let onex = Vector2D(1.0, 0.0)
     static let oney = Vector2D(0.0, 1.0)
     member v.DX = dx
     member v.DY = dy
     /// Get the zero vector
     static member Zero = zero
     /// Get a constant vector along the X axis of length one
     static member OneX = onex
     /// Get a constant vector along the Y axis of length one
     static member OneY = oney

/// working with Indexer Properties
open System.Collections.Generic
type SparseVector(items : seq<int * float>)=
    let elems = new SortedDictionary<_,_>()
    do items |> Seq.iter (fun (k, v) -> elems.Add(k, v))
    /// This defines an indexer property
    member t.Item
        with get(idx) =
            if elems.ContainsKey(idx) then elems.[idx]
            else 0.0

type Vector2DWithOperators(dx : float,dy : float) =
    member x.DX = dx
    member x.DY = dy
    static member (+) (v1 : Vector2DWithOperators, v2 : Vector2DWithOperators) =
         Vector2DWithOperators(v1.DX + v2.DX, v1.DY + v2.DY)
    static member (-) (v1 : Vector2DWithOperators, v2 : Vector2DWithOperators) =
    Vector2DWithOperators (v1.DX - v2.DX, v1.DY - v2.DY)

/// Optional Arguments

open System.Drawing
type LabelInfo(?text : string, ?font : Font) =
    let text = defaultArg text ""
    let font = match font with
               | None -> new Font(FontFamily.GenericSansSerif, 12.0f)
               | Some v -> v
    member x.Text = text
    member x.Font = font
    /// Define a static method which creates an instance
    static member Create(?text, ?font) = new LabelInfo(?text=text, ?font=font)

///
type Interval(lo, hi) =
    member r.Lo = lo
    member r.Hi = hi
    member r.IsEmpty = hi <= lo
    member r.Contains v = lo < v && v < hi
    static member Empty = Interval(0.0, 0.0)
    /// Return the smallest interval that covers both the intervals
    /// This method is overloaded.
    static member Span (r1 : Interval, r2 : Interval) =
        if r1.IsEmpty then r2 else
        if r2.IsEmpty then r1 else
        Interval(min r1.Lo r2.Lo, max r1.Hi r2.Hi)
     /// Return the smallest interval that covers all the intervals
     /// This method is overloaded.
    static member Span(ranges : seq<Interval>) =
        Seq.fold (fun r1 r2 -> Interval.Span(r1, r2)) Interval.Empty ranges

/// listing 6-4 An Object Type with State
type MutableVector2D(dx : float, dy : float) =
    let mutable currDX = dx
    let mutable currDY = dy
    member vec.DX with get() = currDX and set v = currDX <- v
    member vec.DY with get() = currDY and set v = currDY <- v
    member vec.Length
         with get () = sqrt (currDX * currDX + currDY * currDY)
         and set len =
            let theta = vec.Angle
            currDX <- cos theta * len
            currDY <- sin theta * len
    member vec.Angle
         with get () = atan2 currDY currDX
         and set theta =
            let len = vec.Length
            currDX <- cos theta * len
            currDY <- sin theta * len

/// how to use that ??
open System.Collections.Generic
type IntegerMatrix(rows : int, cols : int)=
    let elems = Array2D.zeroCreate<int> rows cols
    /// This defines an indexer property with getter and setter
    member t.Item
       with get (idx1, idx2) = elems.[idx1, idx2]
       and set (idx1, idx2) v = elems.[idx1, idx2] <- v

let x = new IntegerMatrix(5,5);;
x.[2,2] <-2;;

open System.Drawing
type LabelInfoWithPropertySetting() =
    let mutable text = "" // the default
    let mutable font = new Font(FontFamily.GenericSansSerif, 12.0f)
    member x.Text with get() = text and set v = text <- v
    member x.Font with get() = font and set v = font <- v
///
let myWriteStringToFile () =
    let outp = File.CreateText("playlist.txt")
    try
        outp.WriteLine("Enchanted")
        outp.WriteLine("Put your records on")
    finally
        (outp :> System.IDisposable).Dispose()
///
/// With the exception of stack and memory, all objects that own resources should be subtypes of the .
/// NET type System.IDisposable. This is the primary way you can recognize primitive resources and objects
/// that wrap resources. The System.IDisposable interface has a single method; in F# syntax, it can be defined
/// as:
namespace System
    type IDisposable =
        abstract Dispose : unit -> unit

open System.IO
type LineChooser(fileName1, fileName2) =
    let file1 = File.OpenText(fileName1)
    let file2 = File.OpenText(fileName2)
    let rnd = new System.Random()
    let mutable disposed = false
    let cleanup() =
        if not disposed then
            disposed <- true;
            file1.Dispose();
            file2.Dispose();
    interface System.IDisposable with
         member x.Dispose() = cleanup()
    member obj.CloseAll() = cleanup()
    member obj.GetLine() =
        if not file1.EndOfStream &&
            (file2.EndOfStream || rnd.Next() % 2 = 0) then file1.ReadLine()
        elif not file2.EndOfStream then file2.ReadLine()
        else raise (new EndOfStreamException())

/// cleaning up a wrapper p.140: ticket generator
open System
type TicketGenerator() =
    let mutable free = []
    let mutable max = 0
    member h.Alloc() =
        match free with
        | [] -> max <- max + 1; max
        | h :: t -> free <- t; h
    member h.Dealloc(n:int) =
        printfn "returning ticket %d" n
        free <- n :: free
let ticketGenerator = new TicketGenerator()
type Customer() =
    let myTicket = ticketGenerator.Alloc()
    let mutable disposed = false
    let cleanup() =
         if not disposed then
             disposed <- true
             ticketGenerator.Dealloc(myTicket)
    member x.Ticket = myTicket
    interface IDisposable with
        member x.Dispose() = cleanup(); GC.SuppressFinalize(x)
    override x.Finalize() = cleanup()

/// extending a module

module NumberTheoryExtensions =
    let isPrime i =
        let lim = int (sqrt (float i))
        let rec check j =
            j > lim || (i % j <> 0 && check (j + 1))
        check 2
    type System.Int32 with
         member i.IsPrime = isPrime i

module List =
    let rec pairwise l =
         match l with
         | [] | [_] -> []
         | h1 :: ((h2 :: _) as t) -> (h1, h2) :: pairwise t

/////// Chapter 7
/// Listing 7-1. Implementing Objects with Encapsulated State

type IPeekPoke =
    abstract member Peek : unit -> int
    abstract member Poke : int -> unit
let makeCounter initialState =
     let state = ref initialState // referencing a cell
     {new IPeekPoke with
         member x.Poke n = state := !state + n
         member x.Peek() = !state}

/// Listing 7-2. A Type for Objects with Encapsulated State
type TicketGenerator() =
     // Note: let bindings in a type definition are implicitly private to the object
     // being constructed. Members are implicitly public.
     let mutable count = 0
     member x.Next() =
         count <- count + 1;
         count
     member x.Reset () =
         count <- 0
/// how to use that ?

type IStatistic<'T,'U> =
    abstract Record : 'T -> unit
    abstract Value : 'U
let makeAverager(toFloat: 'T -> float) =
    let count = ref 0
    let total = ref 0.0
    {new IStatistic<'T, float> with
          member stat.Record(x) = incr count; total := !total + toFloat x
          member stat.Value = (!total / float !count)}


module public VisitorCredentials =
    /// The internal table of permitted visitors and the
    /// days they are allowed to visit.
    let private visitorTable =
        dict [("Anna", set [DayOfWeek.Tuesday; DayOfWeek.Wednesday]);
              ("Carolyn", set [DayOfWeek.Friday])]
    /// This is the function to check if a person is a permitted visitor.
    /// Note: this is public and can be used by external code
    let public checkVisitor(person) =
        visitorTable.ContainsKey(person) &&
        visitorTable.[person].Contains(DateTime.Today.DayOfWeek)
    /// This is the function to return all known permitted visitors.
    /// Note: this is internal and can be used only by code in this assembly.
    let internal allKnownVisitors() =
        visitorTable.Keys

/// open VisitorCredentials /// before using allKnownVisitors();; for example

/// Listing 7-4. Protecting Internal State Using Accessibility Annotations
/// je ne comprends pas

module public GlobalClock =
    type TickTock = Tick | Tock
    let mutable private clock = Tick
    let private tick = new Event<TickTock>()
    let internal oneTick() =
        (clock <- match clock with Tick -> Tock | Tock -> Tick);
        tick.Trigger (clock)
    let tickEvent = tick.Publish
module internal TickTockDriver =
    open System.Threading
    let timer = new Timer(callback=(fun _ -> GlobalClock.oneTick()),
                          state = null, dueTime = 0, period = 100)

/// Listing 7-5. Making Property Setters Internal to a Type Definition
open System.Collections.Generic
type public SparseVector () =
    let elems = new SortedDictionary<int, float>()
    member internal vec.Add (k, v) = elems.Add(k ,v)
    member public vec.Count = elems.Keys.Count
    member vec.Item
        with public get i =
            if elems.ContainsKey(i) then elems.[i]
            else 0.0
        and internal set i v =
            elems.[i] <- v

type Vector2D =
    {DX : float; DY : float}
module Vector2DOps =
    let length v = sqrt (v.DX * v.DX + v.DY * v.DY)
    let scale k v = {DX = k * v.DX; DY = k * v.DY}
    let shiftX x v = {v with DX = v.DX + x}
    let shiftY y v = {v with DY = v.DY + y}
    let shiftXY (x, y) v = {DX = v.DX + x; DY = v.DY + y}
    let zero = {DX = 0.0; DY = 0.0}
    let constX dx = {DX = dx; DY = 0.0}
    let constY dy = {DX = 0.0; DY = dy}

////// Chapter 8 Working with Textual Data
let line = "Smith, John, 20 January 1986, Software Developer";;

let splitLine (line : string) =
    line.Split [|','|] |> Array.map (fun s -> s.Trim())
let parseEmployee (line : string) =
    match splitLine line with
    | [|last; first; startDate; title|] ->
        last, first, System.DateTime.Parse(startDate), title
    | _ ->
        failwithf "invalid employee format: '%s'" line

open System.IO
File.WriteAllLines("employees.txt", Array.create 10000 line)
let readEmployees (fileName : string) =
    fileName |> File.ReadLines |> Seq.map parseEmployee
let firstThree = readEmployees "employees.txt" |> Seq.truncate 3 |> Seq.toList

/// REGEX
open System.Text.RegularExpressions
let regex s = new Regex(s)
let (=~) s (re:Regex) = re.IsMatch(s)
let (<>~) s (re:Regex) = not (s =~ re)
let samplestring = "This is a string";;
if samplestring =~ regex "his" then
     printfn "A Match! ";;
"This is a string" =~ regex "(is )+";;

let entry = @"
Jolly Jethro
13 Kings Parade
Cambridge, Cambs CB2 1TJ
"
let re =
 regex @"(?<=\n)\s*(?<city>[^\n]+)\s*,\s*(?<county>\w+)\s+(?<pcode>.{3}\s*.{3}).*$"
let r = re.Match(entry);;
r.Groups.["pcode"].Value;;

///

type Term =
    | Term of int * string * int
    | Const of int
type Polynomial = Term list

type Token =
    | ID of string
    | INT of int
    | HAT
    | PLUS
    | MINUS
let tokenR = regex @"((?<token>(\d+|\w+|\^|\+|-))\s*)*"
let tokenize (s : string) =
    [for x in tokenR.Match(s).Groups.["token"].Captures do
         let token =
             match x.Value with
             | "^" -> HAT
             | "-" -> MINUS
             | "+" -> PLUS
             | s when System.Char.IsDigit s.[0] -> INT (int s)
             | s -> ID s
         yield token]

/////// Chapter 9 - Working with Sequences and Structured Data

open System.IO
let rec allFiles dir =
    Seq.append
        (dir |> Directory.GetFiles)
        (dir |> Directory.GetDirectories |> Seq.map allFiles |> Seq.concat)

let checkerboardCoordinates n =
    seq { for row in 1 .. n do
             for col in 1 .. n do
                 let sum = row + col
                 if sum % 2 = 0 then
                     yield (row, col)}

checkerboardCoordinates 3;;

let fileInfo dir =
    seq { for file in Directory.GetFiles dir do
            let creationTime = File.GetCreationTime file
            let lastAccessTime = File.GetLastAccessTime file
            yield (file, creationTime, lastAccessTime)}

let rec allFiles dir =
seq { for file in Directory.GetFiles dir do
          yield file
      for subdir in Directory.GetDirectories dir do
          yield! allFiles subdir}

/// A table of people in our startup
let people =
    [("Amber", 27, "Design")
     ("Wendy", 35, "Events")
     ("Antonio", 40, "Sales")
     ("Petra", 31, "Design")
     ("Carlos", 34, "Marketing")]
/// Extract information from the table of people
let namesOfPeopleStartingWithA =
     people
       |> Seq.map (fun (name, age, dept) -> name)
       |> Seq.filter (fun name -> name.StartsWith "A")
       |> Seq.toList
/// Extract the names of designers from the table of people
let namesOfDesigners =
     people
       |> Seq.filter (fun (_, _, dept) -> dept = "Design")
       |> Seq.map (fun (name, _, _) -> name)
       |> Seq.toList

// Take the first 10 numbers and build a triangle 1, 1, 2, 1, 2, 3, 1, 2, 3, 4, …
let triangleNumbers =
    [ 1 .. 10 ]
      |> Seq.collect (fun i -> [ 1 .. i ] )
      |> Seq.toList

/// A random-number generator
let rand = System.Random()

let gameBoard =
    [ for i in 0 .. 7 do
          for j in 0 .. 7 do
              yield (i,j,rand.Next(10)) ]

let evenPositions =
   gameBoard
     |> Seq.choose (fun (i,j,v) -> if v % 2 = 0 then Some (i,j) else None)
     |> Seq.toList

let firstElementScoringZero =
gameBoard |> Seq.tryFind (fun (i, j, v) -> v = 0)

let firstPositionScoringZero =
     gameBoard |> Seq.tryPick (fun (i, j, v) -> if v = 0 then Some(i, j) else None);;

let positionsGroupedByGameValue =
    gameBoard
        |> Seq.groupBy (fun (i, j, v) -> v)
        |> Seq.sortBy (fun (k, v) -> k)
        |> Seq.toList
let positionsIndexedByGameValue =
   gameBoard
       |> Seq.groupBy (fun (i, j, v) -> v)
       |> Seq.sortBy (fun (k, v) -> k)
       |> Seq.map (fun (k, v) -> (k, Seq.toList v))
       |> dict
let worstPositions = positionsIndexedByGameValue.[0]
let bestPositions = positionsIndexedByGameValue.[9]

Seq.fold (fun acc x -> acc + x) 0.0 [4.0; 5.0; 6.0];;
Seq.fold (+) 0.0 [4.0; 5.0; 6.0];;

List.foldBack (fst >> min) [(3, "three"); (5, "five")] System.Int32.MaxValue;;

open System.IO
let firstTwoLines file =
    seq {use s = File.OpenText(file)
         yield s.ReadLine()
         yield s.ReadLine()}
File.WriteAllLines("test1.txt", [|"Es kommt ein Schiff";
                                  "A ship is coming"|]);;
let twolines() = firstTwoLines "test1.txt";;
//At this point, the file hasn’t yet been opened, and no lines have been read from the file. If you now
//iterate the sequence expression, the file is opened, the first two lines are read, and the results are
//consumed from the sequence and printed. Most important, the file has now also been closed, because the
//Seq.iter aggregate operator is careful to dispose of the underlying enumerator it uses for the sequence,
//which in turn disposes of the file handle generated by File.OpenText:

twolines() |> Seq.iter (printfn "line = '%s'")

/// Working with Abstract Syntax Trees (j'y capte rien)

[<Struct>]
type Complex(r : float, i : float) =
    static member Polar(mag, phase) = Complex(mag * cos phase, mag * sin phase)
    member x.Magnitude = sqrt(r * r + i * i)
    member x.Phase = atan2 i r
    member x.RealPart = r
    member x.ImaginaryPart = i

let (|Rect|) (x : Complex) = (x.RealPart, x.ImaginaryPart)
let (|Polar|) (x : Complex) = (x.Magnitude, x.Phase)

let addViaRect a b =
    match a, b with
    | Rect (ar, ai), Rect (br, bi) -> Complex (ar + br, ai + bi)
let mulViaRect a b =
    match a, b with
    | Rect (ar, ai), Rect (br, bi) -> Complex (ar * br - ai * bi, ai * br + bi * ar)
let mulViaPolar a b =
    match a, b with
    | Polar (m, p), Polar (n, q) -> Complex.Polar (m * n, p + q)
fsi.AddPrinter (fun (c : Complex) -> sprintf "%gr + %gi" c.RealPart c.ImaginaryPart);;

/// Listing 9-3 Defining an active pattern for matching on System.Type

let (|Named|Array|Ptr|Param|) (typ : System.Type) =
    if typ.IsGenericType
    then Named(typ.GetGenericTypeDefinition(), typ.GetGenericArguments())
    elif typ.IsGenericParameter then Param(typ.GenericParameterPosition)
    elif not typ.HasElementType then Named(typ, [||])
    elif typ.IsArray then Array(typ.GetElementType(), typ.GetArrayRank())
    elif typ.IsByRef then Ptr(true, typ.GetElementType())
    elif typ.IsPointer then Ptr(false, typ.GetElementType())
    else failwith "MSDN says this can't happen"

open System
let rec formatType typ =
    match typ with
    | Named (con, [||]) -> sprintf "%s" con.Name
    | Named (con, args) -> sprintf "%s<%s>" con.Name (formatTypes args)
    | Array (arg, rank) -> sprintf "Array(%d,%s)" rank (formatType arg)
    | Ptr(true, arg) -> sprintf "%s&" (formatType arg)
    | Ptr(false, arg) -> sprintf "%s*" (formatType arg)
    | Param(pos) -> sprintf "!%d" pos
and formatTypes typs =
    String.Join(",", Array.map formatType typs)

let rec freeVarsAcc typ acc =
    match typ with
    | Array (arg, rank) -> freeVarsAcc arg acc
    | Ptr (_, arg) -> freeVarsAcc arg acc
    | Param _ -> (typ :: acc)
    | Named (con, args) -> Array.foldBack freeVarsAcc args acc
let freeVars typ = freeVarsAcc typ []
/// trying this:
let x = c.GetType();;
formatType x;;
formatTypes [|x;x|];;
freeVars x;;

let (|MulSeven|_|) inp = if inp % 7 = 0 then Some(inp / 7) else None
let (|MulN|_|) n inp = if inp % n = 0 then Some(inp / n) else None

/// Listing 9-2 Memoizing the construction of abstract syntax tree nodes

type Prop =
    | Prop of int
and PropRepr =
    | AndRepr of Prop * Prop
    | OrRepr of Prop * Prop
    | NotRepr of Prop
    | VarRepr of string
    | TrueRepr
open System.Collections.Generic
module PropOps =
    let internal uniqStamp = ref 0
    type internal PropTable() =
        let fwdTable = new Dictionary<PropRepr, Prop>(HashIdentity.Structural)
        let bwdTable = new Dictionary<int, PropRepr>(HashIdentity.Structural)
        member t.ToUnique repr =
            if fwdTable.ContainsKey repr then fwdTable.[repr]
            else let stamp = incr uniqStamp; !uniqStamp
                 let prop = Prop stamp
                 fwdTable.Add (repr, prop)
                 bwdTable.Add (stamp, repr)
                 prop
        member t.FromUnique (Prop stamp) =
           bwdTable.[stamp]
    let internal table = PropTable ()
    // Public construction functions
    let And (p1, p2) = table.ToUnique (AndRepr (p1, p2))
    let Not p = table.ToUnique (NotRepr p)
    let Or (p1, p2) = table.ToUnique (OrRepr (p1, p2))
    let Var p = table.ToUnique (VarRepr p)
    let True = table.ToUnique TrueRepr
    let False = Not True
    // Deconstruction function
    let getRepr p = table.FromUnique p
    let (|And|Or|Not|Var|True|) prop =
        match table.FromUnique prop with
        | AndRepr (x, y) -> And (x, y)
        | OrRepr (x, y) -> Or (x, y)
        | NotRepr x -> Not x
        | VarRepr v -> Var v
        | TrueRepr -> True

let rec showProp precedence prop =
    let parenIfPrec lim s = if precedence < lim then "(" + s + ")" else s
    match prop with
    | Or (p1, p2) -> parenIfPrec 4 (showProp 4 p1 + " || " + showProp 4 p2)
    | And (p1, p2) -> parenIfPrec 3 (showProp 3 p1 + " && " + showProp 3 p2)
    | Not p -> parenIfPrec 2 ("not " + showProp 1 p)
    | Var v -> v
    | True -> "T"

let rec nnf sign prop =
    match prop with
    | And (p1, p2) ->
        if sign then And (nnf sign p1, nnf sign p2)
        else Or (nnf sign p1, nnf sign p2)
    | Or (p1, p2) ->
        if sign then Or (nnf sign p1, nnf sign p2)
        else And (nnf sign p1, nnf sign p2)
    | Not p ->
        nnf (not sign) p
    | Var _ | True ->
        if sign then prop else Not prop
let NNF prop = nnf true prop

/// Listing 9-5. Customizing equality, hashing, and comparison for a record type definition
/// A type abbreviation indicating we're using integers for unique stamps
/// on objects
type stamp = int
/// A structural type containing a function that can't be compared for equality
[<CustomEquality; CustomComparison>]
type MyThing =
    {Stamp : stamp;
     Behaviour : (int -> int)}
     
     override x.Equals(yobj) =
         match yobj with
         | :? MyThing as y -> (x.Stamp = y.Stamp)
         | _ -> false
     override x.GetHashCode() = hash x.Stamp
     interface System.IComparable with
       member x.CompareTo yobj =
           match yobj with
           | :? MyThing as y -> compare x.Stamp y.Stamp
           | _ -> invalidArg "yobj" "cannot compare values of different types"
/// Listing 9-6. Customizing generic hashing and comparison on a union type
let inline equalsOn f x (yobj : obj) =
    match yobj with
    | :? 'T as y -> (f x = f y)
    | _ -> false

let inline hashOn f x = hash (f x)

let inline compareOn f x (yobj : obj) =
    match yobj with
    | :? 'T as y -> compare (f x) (f y)
    | _ -> invalidArg "yobj" "cannot compare values of different types"

type stamp = int

[<CustomEquality; CustomComparison>]
type MyUnionType =
    | MyUnionType of stamp * (int -> int)
    static member Stamp (MyUnionType (s, _)) = s
    override x.Equals y = equalsOn MyUnionType.Stamp x y
    override x.GetHashCode() = hashOn MyUnionType.Stamp x
    interface System.IComparable with
        member x.CompareTo y = compareOn MyUnionType.Stamp x y
[<ReferenceEquality>]
type MyFormWrapper = MyFormWrapper of System.Windows.Forms.Form * (int -> int)

/// recursion

let rec mapAcc f inputList acc =
    match inputList with
    | [] -> List.rev acc
    | h::t -> mapAcc f t (f h :: acc)

type Chain =
    | ChainNode of int * string * Chain
    | ChainEnd of string
    member chain.LengthNotTailRecursive =
        match chain with
        | ChainNode(_, _, subChain) -> 1 + subChain.LengthNotTailRecursive
        | ChainEnd _ -> 0

type Tree =
    | Node of string * Tree * Tree
    | Tip of string

let rec mkBigUnbalancedTree n tree =
    if n = 0 then tree
    else Node("node", Tip("tip"), mkBigUnbalancedTree (n - 1) tree)
let tree1 = Tip("tip")
let tree2 = mkBigUnbalancedTree 15000 tree1
let tree3 = mkBigUnbalancedTree 15000 tree2
let tree4 = mkBigUnbalancedTree 15000 tree3
let tree5 = mkBigUnbalancedTree 15000 tree4
let tree6 = mkBigUnbalancedTree 15000 tree5

let rec sizeNotTailRecursive tree =
    match tree with
    | Tip _ -> 1
    | Node(_, treeLeft, treeRight) ->
        sizeNotTailRecursive treeLeft + sizeNotTailRecursive treeRight
// Listing 9-9. Making a function tail recursive via an explicit continuation

let rec sizeCont tree cont =
    match tree with
    | Tip _ -> cont 1
    | Node(_, treeLeft, treeRight) ->
        sizeCont treeLeft (fun leftSize ->
          sizeCont treeRight (fun rightSize ->
            cont (leftSize + rightSize)))
let size tree = sizeCont tree (fun x -> x)

// Listing 9-10. Combining an accumulator with an explicit continuation
let rec sizeContAcc acc tree cont =
    match tree with
    | Tip _ -> cont (1 + acc)
    | Node (_, treeLeft, treeRight) ->
        sizeContAcc acc treeLeft (fun accLeftSize ->
        sizeContAcc accLeftSize treeRight cont)
let size tree = sizeContAcc 0 tree (fun x -> x)

// Processing Syntax Trees
type Expr =
    | Add of Expr * Expr
    | Bind of string * Expr * Expr
    | Var of string
    | Num of int

type Env = Map<string, int>
let rec eval (env : Env) expr =
    match expr with
    | Add (e1, e2) -> eval env e1 + eval env e2
    | Bind (var, rhs, body) -> eval (env.Add(var, eval env rhs)) body
    | Var var -> env.[var]
    | Num n -> n

/// Listing 9-11. A tail-recursive expression evaluator using continuations
let rec evalCont (env : Env) expr cont =
    match expr with
    | Add (e1, e2) ->
        evalCont env e1 (fun v1 ->
        evalCont env e2 (fun v2 ->
        cont (v1 + v2)))
    | Bind (var, rhs, body) ->
        evalCont env rhs (fun v1 ->
        evalCont (env.Add(var, v1)) body cont)
    | Num n ->
        cont n
    | Var var ->
        cont (env.[var])
let eval env expr = evalCont env expr (fun x -> x)







/////////////////////////
[<EntryPoint>]
let main argv = 
    stats "www.live.com";;
    Console.ReadKey()
    0 // return an integer exit code
