module Utility

type Option<'a> with
    static member coalesce defaultValue option =
        defaultArg option defaultValue
