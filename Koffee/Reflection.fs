module Reflection

open Microsoft.FSharp.Reflection

let GetUnionCaseName value =
    match FSharpValue.GetUnionFields(value, value.GetType()) with
    | case, _ -> case.Name
