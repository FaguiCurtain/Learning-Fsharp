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


let rt = new RandomTicker(1000);;
rt.RandomTick.Add(fun nextInterval -> printfn "Tick, next = %A" nextInterval);;
rt.Start();;
rt.Stop();;

open System.Windows.Forms
