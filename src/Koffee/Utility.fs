[<AutoOpen>]
module Utility

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open System.Linq
open System.Reactive.Linq
open System.Reactive.Concurrency
open FSharp.Control
open Acadian.FSharp
open System.Windows

let clamp minVal maxVal value =
    value |> max minVal |> min maxVal

let inline applyIf cond f value =
    if cond then f value else value

let inline mapFst f (a, b) = (f a, b)
let inline mapSnd f (a, b) = (a, f b)

let runAsync (f: unit -> 'a) = async {
    let ctx = System.Threading.SynchronizationContext.Current
    do! Async.SwitchToNewThread()
    let result = f()
    do! Async.SwitchToContext ctx
    return result
}

module Async =
    let inline tee f (a: Async<_>) =
        Async.map (fun x -> f x; x) a

module String =
    let readableIdentifier str =
        Regex.Replace(str, @"(?<=[a-z])(?=[A-Z\d])", " ")

module Result =
    let inline map2 f a b =
        match a, b with
        | Ok x, Ok y -> Ok (f x y)
        | Error e, _ -> Error e
        | _, Error e -> Error e

    let inline defaultValue value res =
        match res with
        | Ok a -> a
        | Error _ -> value

    let inline ofErrorOption opt =
        match opt with
        | Some a -> Error a
        | None -> Ok ()

type Dictionary<'K, 'V> with
    member this.TryGetValueOption key =
        match this.TryGetValue key with
        | true, value -> Some value
        | _ -> None

module FormatString =
    let date = "yyyy-MM-dd"
    let time = "HH:mm:ss"
    let dateTime = sprintf "%s  %s" date time

module Format =
    let private formatDate (format: string) (dt: System.DateTime) = dt.ToString(format)
    let date = formatDate FormatString.date
    let time = formatDate FormatString.time
    let dateTime = formatDate FormatString.dateTime

    let fileSize size =
        let scaleNames = ["B";"KB";"MB";"GB"]
        let scale level = pown 1024L level
        let scaledStr size level =
            let scaled = if level > 0 then decimal size / decimal (scale level) else decimal size
            let fmt = if scaled < 10.0m && level > 0 then "0.0" else "0"
            (String.format fmt scaled) + " " + scaleNames.[level]
        if size > scale 3 then scaledStr size 3
        else if size > scale 2 then scaledStr size 2
        else if size > scale 1 then scaledStr size 1
        else scaledStr size 0

module Observable =
    let onCurrent (o: IObservable<_>) =
        o.ObserveOn(DispatcherScheduler.Current)

    let throttle (seconds: float) (o: IObservable<_>) =
        o.Throttle(TimeSpan.FromSeconds(seconds))

    let buffer (seconds: float) (o: IObservable<_>) =
        o.Buffer(TimeSpan.FromSeconds(seconds))
        |> Observable.filter (fun l -> l.Count > 0)

type AsyncSeqResultBuilder() =
    let takeUntilError resSeq =
        resSeq |> AsyncSeq.takeWhileInclusive Result.isOk

    member this.Bind (a, f) = asyncSeq.Bind(a, f)
    member this.Bind (r, f) = asyncSeq {
        match r with
        | Ok x -> yield! f x
        | Error e -> yield Error e
    }
    member this.Bind (resSeq: AsyncSeq<Result<_,_>>, f) = asyncSeq {
        let mutable last = None
        for r in resSeq |> takeUntilError do
            yield r
            last <- Some r
        match last with
        | Some (Ok x) -> yield! f x
        | _ -> ()
    }
    member this.Yield x = result.Return x |> asyncSeq.Yield
    member this.YieldFrom x = result.ReturnFrom x |> asyncSeq.Yield
    member this.YieldFrom (x: AsyncSeq<Result<_,_>>) = asyncSeq.YieldFrom x
    member this.Return x = this.YieldFrom (Error x)
    member this.Return (_: unit) = this.Zero ()
    member this.Zero () = asyncSeq.Zero ()
    member this.Delay f = asyncSeq.Delay f
    member this.Combine (x: AsyncSeq<_>, y) = AsyncSeq.append x y |> takeUntilError
    member this.Using (x, f) = asyncSeq.Using (x, f)

let asyncSeqResult = AsyncSeqResultBuilder()

type Rectangle = {
    Left: int
    Top: int
    Width: int
    Height: int
}
with
    member this.Right = this.Left + this.Width
    member this.Bottom = this.Top + this.Height
    member this.Location = (this.Left, this.Top)
    member this.Size = (this.Width, this.Height)

module Rect =
    let ofPairs loc size = { Left = fst loc; Top = snd loc; Width = fst size; Height = snd size }

    let fit container r =
        let fitDim cl cs l s =
            let s = min s cs
            let l = l |> max cl
                      |> min (cl + cs - s)
            (l, s)
        let left, width = (r.Left, r.Width) ||> fitDim container.Left container.Width
        let top, height = (r.Top, r.Height) ||> fitDim container.Top container.Height
        { Left = left
          Top = top
          Width = width
          Height = height }

type EvtHandler(evt: RoutedEventArgs, ?effect: unit -> unit) =
    member this.Handle () =
        evt.Handled <- true
        effect |> Option.iter (fun f -> f ())
