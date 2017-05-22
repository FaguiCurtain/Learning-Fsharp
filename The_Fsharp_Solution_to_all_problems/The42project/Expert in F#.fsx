(* Listing 2-1. Analyzing a string for duplicate words *)


/// Split a string into words at spaces.
let splitAtSpaces (text: string) =
    text.Split ' '
    |> Array.toList
/// Analyze a string for duplicate words.
let wordCount text =
    let words = splitAtSpaces text
    let numWords = words.Length
    let distinctWords = List.distinct words
    let numDups  = numWords - distinctWords.Length
    (numWords, numDups)
/// Analyze a string for duplicate words and display the results.
let showWordCount text =
    let numWords, numDups = wordCount text
    printfn "--> %d words in the text" numWords
    printfn "--> %d duplicate words" numDups
   
let (numWords,numDups) = wordCount "All the king's horses and all the king's men"

showWordCount "Couldn't put Humpty together again"

let powerOfFourPlusTwo n =
    let n = n * n
    let n = n * n
    let n = n + 2
    n

let site1 = ("www.cnn.com", 10)
let site2 = ("news.bbc.com", 5)
let site3 = ("www.msnbc.com", 4)
let sites = (site1, site2, site3)

let url, relevance = site1
let siteA, siteB, siteC = sites

(* Listing 2-3. Using the .NET networking libraries from F# *)

open System.IO
open System.Net
/// Get the contents of the URL via a web request
let http (url: string) =
    let req = WebRequest.Create(url)
    let resp = req.GetResponse()
    let stream = resp.GetResponseStream()
    let reader = new StreamReader(stream)
    let html = reader.ReadToEnd()
    resp.Close()
    html
http "http://news.bbc.co.uk"

// CHAPTER 11

open System.IO
let watcher = new FileSystemWatcher(__SOURCE_DIRECTORY__, EnableRaisingEvents=true)
watcher.Changed.Add(fun args -> printfn "File %s was changed!" args.Name);;
watcher.Dispose();;


// Listing 11-1. Creating a RandomTicker that defines, publishes, and triggers an event
open System
open System.Windows.Forms
type RandomTicker(approxInterval) =
    let timer = new Timer()
    let rnd = new System.Random(99)
    let tickEvent = new Event<int> ()
    let chooseInterval() : int =
        approxInterval + approxInterval / 4 - rnd.Next(approxInterval / 2)
    do timer.Interval <- chooseInterval()
    do timer.Tick.Add(fun args ->
        let interval = chooseInterval()
        tickEvent.Trigger interval;
        timer.Interval <- interval)
    member x.RandomTick = tickEvent.Publish
    member x.Start() = timer.Start()
    member x.Stop() = timer.Stop()
    interface IDisposable with
        member x.Dispose() = timer.Dispose()

let rt = new RandomTicker(1000);;
rt.RandomTick.Add(fun nextInterval -> printfn "Tick, next = %A" nextInterval);;
rt.Start();;
rt.Stop();;


// modified to make it possible to run on MacOS
// Listing 11-1. Creating a RandomTicker that defines, publishes, and triggers an event
open System
open System.Timers

type RandomTicker(approxInterval:int) =
    let timer = new Timer()
    let rnd = new System.Random(99)
    let tickEvent = new Event<int> ()
    let chooseInterval() : int =
        approxInterval + approxInterval / 4 - rnd.Next((approxInterval / 2))
    do timer.Interval <- (float(chooseInterval()))
    do timer.Elapsed.Add(fun args ->
        let interval = chooseInterval()
        tickEvent.Trigger interval;
        timer.Interval <- float interval)
    member x.RandomTick = tickEvent.Publish
    member x.Start() = timer.Start()
    member x.Stop() = timer.Stop()
    interface IDisposable with
        member x.Dispose() = timer.Dispose()

rt.RandomTick |> Observable.add (fun evArgs -> printfn "Tick!");;
rt.RandomTick |> Observable.add (fun evArgs -> printfn "Tick Tick!");;

let rt = new RandomTicker(1000);;
rt.RandomTick.Add(fun nextInterval -> printfn "Tick, next = %A" nextInterval);;
rt.Start();;
rt.Stop();;


// Listing 11-2. Fetching three web pages simultaneously
open System.Net
open System.IO
let museums =
    [ "MOMA", "http://moma.org/";
      "British Museum", "http://www.thebritishmuseum.ac.uk/";
      "Prado", "http://www.museodelprado.es/" ]

let tprintfn fmt =
    printf "[Thread %d]" System.Threading.Thread.CurrentThread.ManagedThreadId;
    printfn fmt

let fetchAsync(nm, url : string) =
    async {
        tprintfn "Creating request for %s..." nm
        let req = WebRequest.Create(url)
        let! resp = req.AsyncGetResponse()
        tprintfn "Getting response stream for %s..." nm
        let stream = resp.GetResponseStream()
        tprintfn "Reading response for %s..." nm
        let reader = new StreamReader(stream)
        let html = reader.ReadToEnd()
        tprintfn "Read %d characters for %s..." html.Length nm
    }
Async.Parallel [for nm, url in museums -> fetchAsync(nm, url)]
    |> Async.Ignore
    |> Async.RunSynchronously

// Listing 11-3. A synchronous image processor

open System.IO
let numImages = 200
let size = 512
let numPixels = size * size
let makeImageFiles () =
    printfn "making %d %dx%d images... " numImages size size
    let pixels = Array.init numPixels (fun i -> byte i)
    for i = 1 to numImages  do
        System.IO.File.WriteAllBytes(sprintf "Image%d.tmp" i, pixels)
    printfn "done."
let processImageRepeats = 20
let transformImage (pixels, imageNum) =
    printfn "transformImage %d" imageNum;
// Perform a CPU-intensive operation on the image. 
    for i in 1 .. processImageRepeats do
        pixels |> Array.map (fun b -> b + 1uy) |> ignore
    pixels |> Array.map (fun b -> b + 1uy)
let processImageSync i =
    use inStream =  File.OpenRead(sprintf "Image%d.tmp" i)
    let pixels = Array.zeroCreate numPixels
    let nPixels = inStream.Read(pixels,0,numPixels);
    let pixels' = transformImage(pixels,i)
    use outStream =  File.OpenWrite(sprintf "Image%d.done" i)
    outStream.Write(pixels',0,numPixels)
let processImagesSync () =
    printfn "processImagesSync...";
    for i in 1 .. numImages do
        processImageSync(i)

let stopWatch = System.Diagnostics.Stopwatch.StartNew()
processImagesSync()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds

       
// Listing 11-4. The async image processor
let processImageAsync i =
    async {
        use inStream = File.OpenRead(sprintf "Image%d.tmp" i)
        let! pixels = inStream.AsyncRead(numPixels)
        let  pixels2 = transformImage(pixels, i)
        use outStream = File.OpenWrite(sprintf "Image%d.done" i)
        do! outStream.AsyncWrite(pixels2)
}
let processImagesAsync() =
    printfn "processImagesAsync...";
    let tasks = [for i in 1 .. numImages -> processImageAsync(i)]
    Async.RunSynchronously (Async.Parallel tasks) |> ignore
    printfn "processImagesAsync finished!"

let stopWatch = System.Diagnostics.Stopwatch.StartNew()
processImagesAsync()
printfn "%f" stopWatch.Elapsed.TotalMilliseconds

//understanding exceptions and cancellations

let failingAsync = async { do failwith "fail" }
Async.RunSynchronously failingAsync

let failingAsyncs = [async {do failwith "fail A"};
                       async {do failwith "fail B"}]

Async.RunSynchronously (Async.Parallel failingAsyncs)

Async.RunSynchronously (Async.Catch failingAsync)

// Listing 11-5. Implementing a counter using an agent
type Agent<'T> = MailboxProcessor<'T>
let counter =
    new Agent<_>(fun inbox ->
        let rec loop n =
            async {printfn "n = %d, waiting..." n
                   let! msg = inbox.Receive()
                   return! loop (n + msg)}
        loop 0)

counter.Start()
counter.Post(1)
counter.Post(2)

// Listing 11-6. Hiding a mailbox and supporting a fetch method /// The internal type of messages for the agent
type internal msg = Increment of int | Fetch of AsyncReplyChannel<int> | Stop

type CountingAgent() =
    let counter = MailboxProcessor.Start(fun inbox ->
         // The states of the message-processing state machine...
        let rec loop n =
             async {let! msg = inbox.Receive()
                    match msg with
                    | Increment m ->
                        // increment and continue...
                        return! loop(n + m)
                    | Stop ->
                        // exit
                        return ()
                    | Fetch replyChannel  ->
                        // post response to reply channel and continue
                        do replyChannel.Reply n
                        return! loop n}
         // The initial state of the message-processing state machine...
        loop(0))
    member a.Increment(n) = counter.Post(Increment n)
    member a.Stop() = counter.Post Stop
    member a.Fetch() = counter.PostAndReply(fun replyChannel -> Fetch replyChannel)

let counter = new CountingAgent()
counter.Increment(1)
counter.Fetch()
counter.Increment(2)
counter.Fetch()

// Listing 11-7. Scanning a mailbox for relevant messages
type Message =
    | Message1
    | Message2 of int
    | Message3 of string
let agent =
    MailboxProcessor.Start(fun inbox ->
        let rec loop() =
            inbox.Scan(function
                | Message1 ->
                   Some (async {do printfn "message 1!"
                                return! loop()})
                | Message2 n ->
                   Some (async {do printfn "message 2!"
                                return! loop()})
                | Message3 _ ->
                   None)
        loop())

agent.Post(Message1)
agent.Post(Message2(100))
agent.Post(Message3("abc"))
agent.CurrentQueueLength // only Message3 are left in the queue

// Listing 11-8. A scalable, controlled, asynchronous web crawler
open System.Collections.Generic
open System.Net
open System.IO
open System.Threading
open System.Text.RegularExpressions
let limit = 10
let linkPat = "href=\s*\"[^\"h]*(http://[^&\"]*)\""
let getLinks (txt:string) =
    [ for m in Regex.Matches(txt,linkPat)  -> m.Groups.Item(1).Value ]

// A type that helps limit the number of active web requests
type RequestGate(n:int) =
    let semaphore = new Semaphore(initialCount=n,maximumCount=n)
    member x.AsyncAcquire(?timeout) =
        async {
            let! ok = Async.AwaitWaitHandle(semaphore,
                                            ?millisecondsTimeout=timeout)
            if ok then
               return
                 { new System.IDisposable with
                     member x.Dispose() =
                         semaphore.Release() |> ignore }
            else
               return! failwith "couldn't acquire a semaphore"
        }
// Gate the number of active web requests
let webRequestGate = RequestGate(5)
// Fetch the URL, and post the results to the urlCollector.
let collectLinks (url:string) =
        async {
            // An Async web request with a global gate
            let! html =
                async {
                    // Acquire an entry in the webRequestGate. Release
                    // it when 'holder' goes out of scope
                    use! holder = webRequestGate.AsyncAcquire()
                    let req = WebRequest.Create(url,Timeout=5)
                    // Wait for the WebResponse
                    use! response = req.AsyncGetResponse()
                    // Get the response stream
                    use reader = new StreamReader(response.GetResponseStream())
                    // Read the response stream (note: a synchronous read)
                    return reader.ReadToEnd()
                }
            // Compute the links, synchronously
            let links = getLinks html
            // Report, synchronously
            printfn "finished reading %s, got %d links" url (List.length links)
            // We're done
            return links
        }
    /// 'urlCollector' is a single agent that receives URLs as messages. It creates new 
    /// asynchronous tasks that post messages back to this object.
let urlCollector =
        MailboxProcessor.Start(fun self ->
            // This is the main state of the urlCollector
            let rec waitForUrl (visited : Set<string>) =
                async {
                   // Check the limit
                       if visited.Count < limit then
                           // Wait for a URL...
                           let! url = self.Receive()
                           if not (visited.Contains(url)) then
                              // Start off a new task for the new url. Each collects 
                              // links and posts them back to the urlCollector.
                              do! Async.StartChild
                                  (async { let! links = collectLinks url
                                           for link in links do
                                              self.Post link }) |> Async.Ignore
                           return! waitForUrl(visited.Add(url))
                       }
            // This is the initial state.
            waitForUrl(Set.empty))

urlCollector.Post "http://news.google.com";;


// Listing 11-9. A basic implementation of a fork-join parallel operator
let forkJoinParallel(taskSeq) =
    Async.FromContinuations (fun (cont, econt, ccont) ->
        let tasks = Seq.toArray taskSeq
        let count = ref tasks.Length
        let results = Array.zeroCreate tasks.Length
        tasks |> Array.iteri (fun i p ->
            Async.Start
               (async {let! res = p
                       results.[i] <- res;
                       let n = System.Threading.Interlocked.Decrement(count)
                       if n = 0 then cont results})))

open System.Threading
let t = new Thread(ThreadStart(fun _ ->
                printfn "Thread %d: Hello" Thread.CurrentThread.ManagedThreadId));

t.Start()
printfn "Thread %d: Waiting!" Thread.CurrentThread.ManagedThreadId
t.Join()
printfn "Done!"

// Listing 11-10. Shared-memory code with a race condition
open System.Threading
open System.Threading.Tasks
// Simulate doing something
let doSomething() =
    printfn "doing something..."
    System.Threading.Thread.Sleep 100
    printfn "done something..."
// Creating a task
let task = Task.Run (fun () -> doSomething())
// A cancellable operation
let doSomethingCancellable(ct:CancellationToken) =
    printfn "doing something..."
    ct.ThrowIfCancellationRequested()
    System.Threading.Thread.Sleep 100
    ct.ThrowIfCancellationRequested()
    printfn "done something..."
// Create a handle for cancellation
let cts = new CancellationTokenSource()
// Start the task...
let task2 = Task.Run (fun () -> doSomethingCancellable(cts.Token))
// Attempt to cancel the task
cts.Cancel()

// Listing 11-11. Shared-memory code with a race condition

type MutablePair<'T, 'U>(x : 'T, y : 'U) =
    let mutable currentX = x
    let mutable currentY = y
    member p.Value = (currentX, currentY)
    member p.Update(x, y) =
        // Race condition: This pair of updates is not atomic
        currentX <- x
        currentY <- y
let p = new MutablePair<_, _>(1, 2)
do Async.Start (async {do (while true do p.Update(10, 10))})
do Async.Start (async {do (while true do p.Update(20, 20))})
p.Value;

open System.Threading
let lock (lockobj : obj) f  =
    Monitor.Enter lockobj
    try
       f() 
    finally
        Monitor.Exit lockobj

do Async.Start (async { do (while true do lock p (fun () -> p.Update(10,10))) })
do Async.Start (async { do (while true do lock p (fun () -> p.Update(20,20))) })
p.Value;