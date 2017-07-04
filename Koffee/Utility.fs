module Utility

open System.Text.RegularExpressions
open System.Linq

type Option<'a> with
    static member coalesce defaultValue option =
        defaultArg option defaultValue

module Str =
    let ifEmpty fallback str =
        if System.String.IsNullOrEmpty str then fallback
        else str

    let readableIdentifier str =
        Regex.Replace(str, @"(?<=[a-z])(?=[A-Z\d])", " ")

module Order =
    let by f s = Enumerable.OrderBy(s, (fun x -> f x))
    let byDesc f s = Enumerable.OrderByDescending(s, (fun x -> f x))
    let thenBy f s = Enumerable.ThenBy(s, (fun x -> f x))
    let thenByDesc f s = Enumerable.ThenByDescending(s, (fun x -> f x))
