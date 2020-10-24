[<AutoOpen>]
module Utility

open System
open System.Text.RegularExpressions
open System.Linq
open System.Reactive.Linq
open System.Reactive.Concurrency
open FSharp.Control
open Acadian.FSharp

module String =
    let readableIdentifier str =
        Regex.Replace(str, @"(?<=[a-z])(?=[A-Z\d])", " ")

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

    type ISubstitutionProvider =
        abstract member getSubstitution : key:string -> string option

    type EnvSubstitutionProvider() =
        static member instance = new EnvSubstitutionProvider()

        interface ISubstitutionProvider with
            member _.getSubstitution key =
                Environment.GetEnvironmentVariable(key, EnvironmentVariableTarget.Process)
                |> Option.ofObj
        
    /// Substitutes variables in string with percent syntax.
    /// Example input: "Hello %target%!"
    /// Outputs: "Hello world!"
    /// Where the "target" maps to "world"
    let subVars (subs: ISubstitutionProvider) str =
        str

    /// Wrapper of subVars that uses EnvSubstitutionProvider
    let subEnvVars str =
        subVars EnvSubstitutionProvider.instance str

module Observable =
    let throttle (seconds: float) (o: IObservable<_>) =
        o.Throttle(TimeSpan.FromSeconds(seconds))

    let onCurrent (o: IObservable<_>) =
        o.ObserveOn(DispatcherScheduler.Current)

    let buffer (seconds: float) (o: IObservable<_>) =
        o.Buffer(TimeSpan.FromSeconds(seconds))

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
    member this.Zero () = result.Zero () |> asyncSeq.Return
    member this.Delay f = asyncSeq.Delay f
    member this.Combine (x: AsyncSeq<_>, y) = AsyncSeq.append x y |> takeUntilError
    member this.Using (x, f) = asyncSeq.Using (x, f)

let asyncSeqResult = AsyncSeqResultBuilder()

module Order =
    let by f s = Enumerable.OrderBy(s, (fun x -> f x))
    let byDesc f s = Enumerable.OrderByDescending(s, (fun x -> f x))
    let thenBy f s = Enumerable.ThenBy(s, (fun x -> f x))
    let thenByDesc f s = Enumerable.ThenByDescending(s, (fun x -> f x))

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
