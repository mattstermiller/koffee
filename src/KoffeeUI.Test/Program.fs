module Program

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Data
open System.Windows.Input
open System.Windows.Media
open ColorPicker
open KoffeeUI

type Clipboard = System.Windows.Forms.Clipboard

type Item = {
    Name: string
    TypeName: string
    Modified: string
    Size: string
    IsHidden: bool
}

type HistoryPanelRow = {
    Header: string
    Content: string
    IsListTraversable: bool
    IsCurrent: bool
}
with
    static member create isCurrent (header, content) = {
        Header = header
        Content = content
        IsListTraversable = true
        IsCurrent = isCurrent
    }

type KeyEventArgs with
    member this.RealKey = if this.Key = Key.System then this.SystemKey else this.Key
    member this.Chord = (Keyboard.Modifiers, this.RealKey)

let addCloseHotKey (window: Window) =
    window.PreviewKeyDown.Add (fun e ->
        if e.Chord = (ModifierKeys.Control, Key.W) then
            e.Handled <- true
            window.Close()
    )

let setupWindow (window: MainWindow) =
    let show (e: UIElement) = e.Visibility <- Visibility.Visible

    let grid = window.ItemGrid

    let addColumn name autoWidth =
        let col = DataGridTextColumn()
        col.Header <- name
        col.Binding <- Binding(name)
        let widthType = if autoWidth then DataGridLengthUnitType.Auto else DataGridLengthUnitType.Star
        col.Width <- DataGridLength(1.0, widthType)
        grid.Columns.Add col

    addColumn "Name" false
    addColumn "Modified" true
    addColumn "Size" true

    let folders =
        List.init 4 (fun i ->
            let n = i + 1
            {
                Name = sprintf "Folder %i" n
                TypeName = "Folder"
                Modified = ""
                Size = ""
                IsHidden = n = 4
            }
        )
    let files =
        List.init 10 (fun i ->
            let n = i + 1
            {
                Name = sprintf "File %i" n
                TypeName = "File"
                Modified = sprintf "2000/1/%i" (10 + n)
                Size = sprintf "%i KB" (n * 10)
                IsHidden = n = 10
            }
        )
    grid.ItemsSource <- folders @ files
    grid.SelectedIndex <- 4
    let mutable selectedItems = files.[4..7]
    grid.Tag <- selectedItems

    grid.PreviewKeyDown.Add (fun e ->
        match e.Chord with
        | ModifierKeys.None, Key.J -> grid.SelectedIndex <- grid.SelectedIndex + 1 |> min grid.Items.Count
        | ModifierKeys.None, Key.K -> grid.SelectedIndex <- grid.SelectedIndex - 1 |> max 0
        | ModifierKeys.None, Key.Space ->
            let cursorItem = unbox grid.SelectedItem
            if selectedItems |> List.contains cursorItem then
                selectedItems <- selectedItems |> List.except [cursorItem]
            else
                selectedItems <- selectedItems @ [cursorItem]
            grid.Tag <- selectedItems
        | _ -> ()
    )

    window.HistoryHeader.Text <- "Navigation History"
    window.HistoryItems.ItemsSource <- [
        HistoryPanelRow.create false ("H", "/c/")
        HistoryPanelRow.create true ("(current)", "/c/path")
        HistoryPanelRow.create false ("L", "/c/path/Folder 1")
    ]
    show window.HistoryPanel

    window.Progress.Value <- 0.3
    show window.Progress

    show window.InputPanel
    show window.YankRegisterPanel

    window.ErrorText.Margin <-
        let margin = window.ErrorText.Margin
        margin.set_Left 50
        margin

    grid.Focus() |> ignore

    window |> addCloseHotKey
    window.WindowStartupLocation <- WindowStartupLocation.CenterScreen
    window.Width <- 660
    window.Title <- "Koffee UI Test"

let inline elem a = a :> UIElement

let addChildren uiElements (panel: Panel) =
    uiElements |> Seq.iter (fun e -> panel.Children.Add e |> ignore)
    panel

type ResourceButton(resourceName, initialColor) as this =
    let swatchBrush = SolidColorBrush()
    let button = RadioButton(GroupName = "Resource", Margin = Thickness(5))
    let selected = Event<Color>()

    do
        this.SetColor initialColor

        button.Content <-
            StackPanel(Orientation = Orientation.Horizontal)
            |> addChildren [
                Border(Background = swatchBrush, Width = 20, Height = 20, Margin = Thickness(0, 0, 5, 0)) |> elem
                TextBlock(Text = resourceName)
            ]
        button.Click.Add (fun _ -> selected.Trigger swatchBrush.Color)
        button.MouseRightButtonDown.Add (fun _ ->
            button.IsChecked <- true
            selected.Trigger initialColor
        )

    member _.ResourceName = resourceName
    member _.Button = button
    member _.Selected = selected.Publish

    member _.SetColor color =
        swatchBrush.Color <- color

let createThemeWindow (resources: ResourceDictionary) =
    let colorPicker = StandardColorPicker()

    let resourceColors =
        resources.Keys
        |> Seq.cast<obj>
        |> Seq.map string
        |> Seq.choose (fun resName ->
            match resources.[resName] with
            | :? Color as c -> Some (resName, c)
            | _ -> None
        )
        |> Seq.sortBy fst
        |> Seq.toArray

    let resourceButtons =
        resourceColors |> Array.map (fun (resName, color) ->
            let rb = ResourceButton(resName, color)
            rb.Selected.Add colorPicker.set_SelectedColor
            rb
        )

    colorPicker.ColorChanged.Add (fun _ ->
        resourceButtons
        |> Array.tryFind (fun rb -> rb.Button.IsChecked = Nullable true)
        |> Option.iter (fun rb ->
            let color = colorPicker.SelectedColor
            rb.SetColor color
            resources.[rb.ResourceName] <- color
        )
    )

    let copyButton = Button(Content = "Copy to Clipboard", Margin = Thickness(5))
    copyButton.Click.Add (fun _ ->
        resourceColors
        |> Seq.map (fun (name, _) ->
            let color = resources.[name] :?> Color
            sprintf "  <Color x:Key=\"%s\">%O</Color>" name color
        )
        |> String.concat "\n"
        |> Clipboard.SetText
    )

    let window = Window(Title = "Theme Editor", Width = 400, Height = 600)
    window.Content <-
        StackPanel(Orientation = Orientation.Horizontal)
        |> addChildren [
            StackPanel(Orientation = Orientation.Vertical)
            |> addChildren ([
                yield! resourceButtons |> Seq.map (fun rb -> rb.Button |> elem)
                copyButton
            ])
            |> elem
            colorPicker
        ]
    window |> addCloseHotKey
    window

[<EntryPoint>]
[<System.STAThread>]
let main args =
    let main = MainWindow()
    setupWindow main

    let resources = main.Resources.MergedDictionaries.[0]
    let theme = createThemeWindow resources

    main.Loaded.Add (fun _ ->
        theme.Left <- main.Left + main.Width
        theme.Top <- main.Top
        theme.Show()
        main.Focus() |> ignore
    )
    theme.Closed.Add (fun _ -> main.Close())

    Application().Run(main) |> ignore
    0
