﻿module UIHelpers

open System.ComponentModel
open System.Windows
open System.Windows.Data
open System.Windows.Controls
open System.Windows.Input
open FSharp.Desktop.UI
open FSharp.Quotations
open ModelExtensions
open Reflection

let onKey key action (evt: KeyEventArgs) =
    if evt.Key = key then
        evt.Handled <- true
        action() |> ignore

let onKeyCombo mods key action (evt: KeyEventArgs) =
    if Keyboard.Modifiers = mods && evt.Key = key then
        evt.Handled <- true
        action() |> ignore

/// Calls the binding function with the current property value and subscribes the binding function to property changes
let bindPropertyToFunc (propertyExpr: Expr<'a>) bindFunc =
    match propertyExpr with
    | PropertyExpression (:? Model as model, property) ->
        property.GetValue model :?> 'a |> bindFunc
        model.OnPropertyChanged propertyExpr bindFunc
    | _ -> failwith "Invalid property expression. It must be a property of this model type."

type DataGrid with
    member this.AddColumn (propName, ?header: string, ?widthWeight, ?alignRight, ?converter: IValueConverter,
                           ?format: string) =
        let headerStr = defaultArg header propName
        let width = defaultArg widthWeight 1.0

        let col = DataGridTextColumn()
        col.Header <- headerStr
        col.Width <- DataGridLength(width, DataGridLengthUnitType.Star)
        if alignRight = Some true then
            col.ElementStyle <- Style(typedefof<TextBlock>)
            col.ElementStyle.Setters.Add(Setter(FrameworkElement.HorizontalAlignmentProperty,
                                                HorizontalAlignment.Right))

        let binding = Binding(propName)
        if converter.IsSome then binding.Converter <- converter.Value
        if format.IsSome then binding.StringFormat <- format.Value
        col.Binding <- binding

        this.Columns.Add col
