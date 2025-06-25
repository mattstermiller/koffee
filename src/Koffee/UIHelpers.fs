module UIHelpers

open System
open System.Runtime.CompilerServices
open System.Windows
open System.Windows.Controls
open System.Windows.Controls.Primitives
open System.Windows.Data
open System.Windows.Input
open System.Windows.Media
open System.Reactive.Linq
open VinylUI
open VinylUI.Wpf
open Microsoft.FSharp.Quotations
open Acadian.FSharp

let (|DigitKey|_|) (key: Key) =
    if key >= Key.D0 && key <= Key.D9 then
        Some (int key - int Key.D0)
    else
        None

type RoutedEventArgs with
    member this.Handler = KeyPressHandler(this)
    member this.HandlerWithEffect f = KeyPressHandler(this, f)

type KeyEventArgs with
    member this.RealKey = if this.Key = Key.System then this.SystemKey else this.Key
    member this.Chord = (Keyboard.Modifiers, this.RealKey)

type UIElement with
    member this.IsHidden
        with get () = this.Visibility = Visibility.Hidden
        and set value = this.Visibility <- if value then Visibility.Hidden else Visibility.Visible

    member this.IsCollapsed
        with get () = this.Visibility = Visibility.Collapsed
        and set value = this.Visibility <- if value then Visibility.Collapsed else Visibility.Visible

type DependencyObject with
    member this.FindVisualChild<'a when 'a :> DependencyObject>() =
        seq {0..VisualTreeHelper.GetChildrenCount(this)-1}
        |> Seq.tryPick (fun i ->
            match VisualTreeHelper.GetChild(this, i) with
            | :? 'a as target -> Some target
            | child -> child.FindVisualChild<'a>()
        )

type CheckBox with
    member this.Toggle () =
        this.IsChecked <- not (this.IsChecked.GetValueOrDefault()) |> Nullable

    member this.CheckedChanged =
        ([  this.Checked
            this.Unchecked
            this.Indeterminate
        ] |> Seq.cast<IObservable<_>>).Merge()

type DataGrid with
    member this.AddColumn (projection: Expr<'a -> 'v>, ?header: string, ?widthWeight, ?alignRight,
                           ?conversion: 'v -> _, ?format: string) =
        let propName =
            match projection with
            | Reflection.PropertySelector prop -> prop.Name
            | _ -> failwith "Projection expression must be a function that returns a property from an item."
        let col = DataGridTextColumn()
        col.Header <- header |? propName
        let widthType = if widthWeight.IsSome then DataGridLengthUnitType.Star else DataGridLengthUnitType.Auto
        col.Width <- DataGridLength(widthWeight |? 1.0, widthType)
        if alignRight = Some true then
            col.ElementStyle <- Style(typedefof<TextBlock>)
            col.ElementStyle.Setters.Add(Setter(FrameworkElement.HorizontalAlignmentProperty,
                                                HorizontalAlignment.Right))

        let binding = Binding(propName)
        conversion |> Option.iter (fun convert ->
            binding.Converter <-
                { new IValueConverter with
                    member this.Convert(value, _, _, _) = value |> unbox<'v> |> convert |> box
                    member this.ConvertBack(value, _, _, _) = value
                }
        )
        format |> Option.iter binding.set_StringFormat
        col.Binding <- binding

        this.Columns.Add col

    member this.ActualColumnHeaderHeight =
        this.FindVisualChild<DataGridColumnHeadersPresenter>() |> Option.map (fun h -> h.ActualHeight) |? 0.0

type DataGridColumn with
    member this.IsCollapsed
        with get () = this.Visibility = Visibility.Collapsed
        and set value = this.Visibility <- if value then Visibility.Collapsed else Visibility.Visible

type Window with
    member this.GetScreen () =
        Forms.Screen.FromHandle(Interop.WindowInteropHelper(this).Handle)

    member this.GetScreenWorkingArea () =
        let area = this.GetScreen().WorkingArea
        { Left = area.Left
          Top = area.Top
          Width = area.Width
          Height = area.Height }

type DataGridScroller(grid: DataGrid) =
    let mutable scrollViewer = None

    member this.ScrollTo topIndex =
        if scrollViewer.IsNone then
            scrollViewer <- grid.FindVisualChild<ScrollViewer>()
        scrollViewer |> Option.iter (fun sv -> sv.ScrollToVerticalOffset (float topIndex))

[<Extension>]
type BindViewPartExtensions() =
    [<Extension>]
    static member toModel(viewPart: BindViewPart<Control, bool Nullable>, modelProperty: Expr<bool>) =
        viewPart.toModel(modelProperty, ((=) (Nullable true)), Nullable)

let onKey key action (evt: KeyEventArgs) =
    if evt.Key = key then
        evt.Handled <- true
        action() |> ignore

let onKeyCombo mods key action (evt: KeyEventArgs) =
    if Keyboard.Modifiers = mods && evt.Key = key then
        evt.Handled <- true
        action() |> ignore

let onKeyFunc key resultFunc (keyEvent : IEvent<KeyEventHandler, KeyEventArgs>) =
    keyEvent |> Observable.choose (fun evt ->
        if evt.Key = key then
            evt.Handled <- true
            Some <| resultFunc()
        else
            None
    )

let isNotModifier (evt: KeyEventArgs) =
    let modifierKeys = [
        Key.LeftShift; Key.RightShift; Key.LeftCtrl; Key.RightCtrl;
        Key.LeftAlt; Key.RightAlt; Key.LWin; Key.RWin; Key.System
    ]
    not <| List.contains evt.RealKey modifierKeys
