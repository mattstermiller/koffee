module Koffee.ValueConverters

open System
open System.Windows.Data
open Microsoft.FSharp.Reflection

type DiscriminatedUnionText() =
    let caseName value =
        match FSharpValue.GetUnionFields(value, value.GetType()) with
        | case, _ -> case.Name

    interface IValueConverter with
        override this.Convert(value, targetType, _, _) =
            value |> caseName |> box

        override this.ConvertBack(_, _, _, _) =
            raise(NotImplementedException())

type DiscriminatedUnionValue() =
    let caseValue index value =
        match FSharpValue.GetUnionFields(value, value.GetType()) with
        | case, _ -> case.GetFields().[index].GetValue(value)

    interface IValueConverter with
        override this.Convert(value, targetType, _, _) =
            value |> caseValue 0 |> box

        override this.ConvertBack(_, _, _, _) =
            raise(NotImplementedException())

