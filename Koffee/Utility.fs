[<AutoOpen>]
module Utility

open System.Text.RegularExpressions
open System.Linq
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
        let format (num: int64) = num.ToString("N0")
        let scale level = pown 1024L level
        let scaleCutoff level = 10L * (scale level)
        let scaledStr size level =
            let scaled = size / (scale level)
            let levelName = "KB,MB,GB".Split(',').[level-1]
            (format scaled) + " " + levelName
        if size > scaleCutoff 3 then scaledStr size 3
        else if size > scaleCutoff 2 then scaledStr size 2
        else if size > scaleCutoff 1 then scaledStr size 1
        else format size + " B"

type AsyncResultBuilder() =
    member this.Bind (a, f) = async.Bind(a, f)
    member this.Bind (r, f) = async {
        match r with
        | Ok x -> return! f x
        | Error e -> return Error e
    }
    member this.Return x = result.Return x |> async.Return
    member this.ReturnFrom x = result.ReturnFrom x |> async.Return
    member this.ReturnFrom (x: Async<Result<_,_>>) = async.ReturnFrom x
    member this.Zero () = result.Zero () |> async.Return
    member this.Delay f = async.Delay f
    member this.Combine (x, y) = async {
        let! res = x
        match res with
        | Ok () -> return! y
        | Error e -> return Error e
    }
    member this.Using (x, f) = async.Using (x, f)

let asyncResult = AsyncResultBuilder()

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
