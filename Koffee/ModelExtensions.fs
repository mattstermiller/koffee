module ModelExtensions

open System.ComponentModel
open Microsoft.FSharp.Quotations
open Reflection

type INotifyPropertyChanged with
    member this.PropertyChangedEvent (propertyExpr: Expr<'a>) =
        match propertyExpr with
        | PropertyExpression (obj, property) when obj.GetType() = this.GetType() ->
            this.PropertyChanged
            |> Observable.filter (fun e -> e.PropertyName = property.Name)
            |> Observable.map (fun e -> property.GetValue this :?> 'a)
        | _ -> failwith "Invalid property expression. It must be a property of this model type."

    member this.OnPropertyChanged propertySelector handler =
        this.PropertyChangedEvent propertySelector |> Observable.add handler
