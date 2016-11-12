module ModelExtensions

open System.ComponentModel
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

type INotifyPropertyChanged with
    member this.OnPropertyChanged (propertySelector : Expr<'a>) =
        match propertySelector with
        | PropertyGet (Some obj, property, []) when obj.Type.IsAssignableFrom(this.GetType()) ->
            this.PropertyChanged
            |> Observable.filter (fun e -> e.PropertyName = property.Name)
            |> Observable.map (fun e -> property.GetValue this :?> 'a)
        | _ -> failwith "Invalid propertySelector"
