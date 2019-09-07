[<AutoOpen>]
module UIHelpers

open System
open System.Windows
open System.Windows.Data
open System.Windows.Controls
open System.Windows.Input
open System.Reactive.Linq
open Acadian.FSharp

let onKey key action (evt: KeyEventArgs) =
    if evt.Key = key then
        evt.Handled <- true
        action() |> ignore

let onKeyCombo mods key action (evt: KeyEventArgs) =
    if Keyboard.Modifiers = mods && evt.Key = key then
        evt.Handled <- true
        action() |> ignore

type EvtHandler(evt: RoutedEventArgs, ?effect: unit -> unit) =
    member this.Handle () =
        evt.Handled <- true
        effect |> Option.iter (fun f -> f ())

type RoutedEventArgs with
    member this.Handler = EvtHandler(this)
    member this.HandlerWithEffect f = EvtHandler(this, f)

type KeyEventArgs with
    member this.RealKey = if this.Key = Key.System then this.SystemKey else this.Key
    member this.Chord = (Keyboard.Modifiers, this.RealKey)

type UIElement with
    member this.Visible
        with get () = this.Visibility = Visibility.Visible
        and set value = this.Visibility <- if value then Visibility.Visible else Visibility.Hidden

type CheckBox with
    member this.Toggle () =
        this.IsChecked <- not (this.IsChecked.GetValueOrDefault()) |> Nullable

    member this.CheckedChanged =
        ([  this.Checked
            this.Unchecked
            this.Indeterminate
        ] |> Seq.cast<IObservable<_>>).Merge()

type DataGrid with
    member this.AddColumn (propName, ?header: string, ?widthWeight, ?alignRight, ?converter: IValueConverter,
                           ?format: string) =
        let col = DataGridTextColumn()
        col.Header <- header |? propName
        let widthType = if widthWeight.IsSome then DataGridLengthUnitType.Star else DataGridLengthUnitType.Auto
        col.Width <- DataGridLength(widthWeight |? 1.0, widthType)
        if alignRight = Some true then
            col.ElementStyle <- Style(typedefof<TextBlock>)
            col.ElementStyle.Setters.Add(Setter(FrameworkElement.HorizontalAlignmentProperty,
                                                HorizontalAlignment.Right))

        let binding = Binding(propName)
        if converter.IsSome then binding.Converter <- converter.Value
        if format.IsSome then binding.StringFormat <- format.Value
        col.Binding <- binding

        this.Columns.Add col

type Window with
    member this.GetScreen () =
        Forms.Screen.FromHandle(Interop.WindowInteropHelper(this).Handle) 

    member this.GetScreenWorkingArea () =
        let area = this.GetScreen().WorkingArea
        { Left = area.Left
          Top = area.Top
          Width = area.Width
          Height = area.Height }
