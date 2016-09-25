module Koffee.ValueConverters

open System.Windows.Data
open Microsoft.FSharp.Reflection

type UnionText() =
    let caseName value =
        match FSharpValue.GetUnionFields(value, value.GetType()) with
        | case, _ -> case.Name

    let makeCase value =
        match FSharpValue.GetUnionFields(value, value.GetType()) with
        | case, _ -> FSharpValue.MakeUnion(case, value)

    interface IValueConverter with
        override this.Convert(value, targetType, _, _) =
            value |> caseName |> box

        override this.ConvertBack(caseName, targetType, _, _) =
            let case =
                FSharpType.GetUnionCases targetType
                |> Array.filter (fun uc -> uc.Name = caseName.ToString())
                |> Array.head
            FSharpValue.MakeUnion(case, null, false)

type UnionValue() =
    let caseValue index value =
        match FSharpValue.GetUnionFields(value, value.GetType()) with
        | case, _ -> case.GetFields().[index].GetValue(value)

    interface IValueConverter with
        override this.Convert(value, targetType, _, _) =
            value |> caseValue 0 |> box

        override this.ConvertBack(value, targetType, _, _) =
            let fieldTypes (case: UnionCaseInfo) =
                case.GetFields() |> Array.map (fun f -> f.PropertyType)
            let expectedTypes = [|value.GetType()|]
            let case =
                FSharpType.GetUnionCases targetType
                |> Array.filter (fun uc -> fieldTypes uc = expectedTypes)
                |> Array.head
            FSharpValue.MakeUnion(case, [|value|], false)
