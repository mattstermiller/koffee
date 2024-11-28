module Reflection

open FSharp.Reflection
open FSharp.Quotations.Patterns
open FSharp.Quotations.Evaluator

let getUnionCaseName value =
    match FSharpValue.GetUnionFields(value, value.GetType()) with
    | case, _ -> case.Name

let rec private enumerateUnionCaseValuesUntyped unionType =
    FSharpType.GetUnionCases unionType
    |> Seq.collect (fun case ->
        let makeUnion value =
            let args = if value = null then null else [|value|]
            FSharpValue.MakeUnion(case, args, false)
        match case.GetFields() with
        | [||] ->
            makeUnion null |> Seq.singleton
        | [|field|] when field.PropertyType = typeof<bool> ->
            [false; true]
            |> Seq.map makeUnion
        | [|field|] when field.PropertyType |> FSharpType.IsUnion ->
            enumerateUnionCaseValuesUntyped field.PropertyType
            |> Seq.map makeUnion
        | _ -> []
    )

let enumerateUnionCaseValues<'Union> =
    enumerateUnionCaseValuesUntyped typeof<'Union> |> Seq.cast<'Union>

let private parseUnionCaseUntyped unionType caseName =
    FSharpType.GetUnionCases unionType
    |> Array.filter (fun uc -> uc.Name = caseName.ToString())
    |> Array.tryHead
    |> Option.map (fun case -> FSharpValue.MakeUnion(case, null, false))

let parseUnionCase<'Union> caseName =
    parseUnionCaseUntyped typedefof<'Union> caseName |> Option.map unbox<'Union>

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
