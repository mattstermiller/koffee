module Utility

type Str =
    static member ifEmpty fallback str =
        if System.String.IsNullOrEmpty str then fallback
        else str

type Option<'a> with
    static member coalesce defaultValue option =
        defaultArg option defaultValue
