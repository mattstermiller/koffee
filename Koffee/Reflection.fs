module Reflection

open FSharp.Reflection
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.Evaluator

let GetUnionCaseName value =
    match FSharpValue.GetUnionFields(value, value.GetType()) with
    | case, _ -> case.Name

let (|PropertyExpression|_|) expr =
    match expr with
    | PropertyGet (Some objExpr, property, []) ->
        Some (QuotationEvaluator.EvaluateUntyped objExpr, property)
    | _ -> None
