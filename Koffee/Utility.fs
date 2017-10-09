module Utility

open System.Text.RegularExpressions
open System.Linq

type Option<'a> with
    static member pair a b =
        match a, b with
        | Some av, Some bv -> Some (av, bv)
        | _ -> None

    static member mapPair f = Option.map (fun (a, b) -> f a, f b)

module Str =
    let ifEmpty fallback str =
        if System.String.IsNullOrEmpty str then fallback
        else str

    let join sep (strs: string seq) = System.String.Join(sep, strs)

    let readableIdentifier str =
        Regex.Replace(str, @"(?<=[a-z])(?=[A-Z\d])", " ")

module FormatString =
    let date = "yyyy-MM-dd"
    let time = "HH:mm"
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
        else format size

module Order =
    let by f s = Enumerable.OrderBy(s, (fun x -> f x))
    let byDesc f s = Enumerable.OrderByDescending(s, (fun x -> f x))
    let thenBy f s = Enumerable.ThenBy(s, (fun x -> f x))
    let thenByDesc f s = Enumerable.ThenByDescending(s, (fun x -> f x))
