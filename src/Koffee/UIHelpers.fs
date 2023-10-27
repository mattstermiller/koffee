[<AutoOpen>]
module UIHelpers

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Controls.Primitives
open System.Windows.Data
open System.Windows.Input
open System.Windows.Media
open System.Reactive.Linq
open System.Windows.Media.Imaging
open Microsoft.FSharp.Quotations
open Acadian.FSharp

let onKey key action (evt: KeyEventArgs) =
    if evt.Key = key then
        evt.Handled <- true
        action() |> ignore

let onKeyCombo mods key action (evt: KeyEventArgs) =
    if Keyboard.Modifiers = mods && evt.Key = key then
        evt.Handled <- true
        action() |> ignore

let (|DigitKey|_|) (key: Key) =
    if key >= Key.D0 && key <= Key.D9 then
        Some (int key - int Key.D0)
    else
        None

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

    member this.Collapsed
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
        
    member this.AddImageColumn (projection: Expr<'a -> BitmapSource>, ?widthWeight) =
        let propName =
            match projection with
            | Reflection.PropertySelector prop -> prop.Name
            | _ -> failwith "Projection expression must be a function that returns a property from an item."
        let col = DataGridTemplateColumn()
        col.Header <- ""
        
        let factory = FrameworkElementFactory(typeof<Image>)

        let binding = Binding(propName)
        
        let cellTemplate = DataTemplate()
        cellTemplate.VisualTree <- factory
        factory.SetValue(Image.SourceProperty, binding)
        
        col.CellTemplate <- cellTemplate
        
        let widthType = if widthWeight.IsSome then DataGridLengthUnitType.Star else DataGridLengthUnitType.Auto
        col.Width <- DataGridLength(widthWeight |? 1.0, widthType)

        this.Columns.Add col

    member this.ActualColumnHeaderHeight =
        this.FindVisualChild<DataGridColumnHeadersPresenter>() |> Option.map (fun h -> h.ActualHeight) |? 0.0

type DataGridColumn with
    member this.Collapsed
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
