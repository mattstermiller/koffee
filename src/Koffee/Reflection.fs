module Reflection

open FSharp.Reflection
open FSharp.Quotations.Patterns
open FSharp.Quotations.Evaluator

let GetUnionCaseName value =
    match FSharpValue.GetUnionFields(value, value.GetType()) with
    | case, _ -> case.Name

let ParseUnionCaseUntyped unionType caseName =
    FSharpType.GetUnionCases unionType
    |> Array.filter (fun uc -> uc.Name = caseName.ToString())
    |> Array.tryHead
    |> Option.map (fun case -> FSharpValue.MakeUnion(case, null, false))

let ParseUnionCase<'Union> caseName =
    ParseUnionCaseUntyped typedefof<'Union> caseName
    |> Option.map (fun c -> c :?> 'Union)

let (|PropertyExpression|_|) expr =
    match expr with
    | PropertyGet (Some objExpr, property, []) ->
        Some (QuotationEvaluator.EvaluateUntyped objExpr, property)
    | _ -> None

let (|PropertySelector|_|) expr =
    match expr with
    | Lambda (_, PropertyGet (_, property, [])) ->
        Some property
    | _ -> None
