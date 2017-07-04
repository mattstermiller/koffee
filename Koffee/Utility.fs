module Utility

open System.Text.RegularExpressions

type Option<'a> with
    static member coalesce defaultValue option =
        defaultArg option defaultValue

module Str =
    let ifEmpty fallback str =
        if System.String.IsNullOrEmpty str then fallback
        else str

    let readableIdentifier str =
        Regex.Replace(str, @"(?<=[a-z])(?=[A-Z\d])", " ")
