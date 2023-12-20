module Program

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Data
open System.Windows.Input
open KoffeeUI

type Item = {
    Name: string
    TypeName: string
    Modified: string
    Size: string
    IsHidden: bool
}

type KeyEventArgs with
    member this.RealKey = if this.Key = Key.System then this.SystemKey else this.Key
    member this.Chord = (Keyboard.Modifiers, this.RealKey)

[<EntryPoint>]
[<System.STAThread>]
let main args =
    let window = MainWindow()

    window.PreviewKeyDown.Add (fun e ->
        if e.Chord = (ModifierKeys.Control, Key.W) then
            window.Close()
    )

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

    let items =
        [
            yield! Array.init 4 (fun i ->
                let n = i + 1
                {
                    Name = sprintf "Folder %i" n
                    TypeName = "Folder"
                    Modified = ""
                    Size = ""
                    IsHidden = n = 4
                }
            )
            yield! Array.init 10 (fun i ->
                let n = i + 1
                {
                    Name = sprintf "File %i" n
                    TypeName = "File"
                    Modified = sprintf "2000/1/%i" (10 + n)
                    Size = sprintf "%i KB" (n * 10)
                    IsHidden = n = 10
                }
            )
        ]
    grid.ItemsSource <- items

    grid.PreviewKeyDown.Add (fun e ->
        match e.Chord with
        | ModifierKeys.None, Key.J -> grid.SelectedIndex <- grid.SelectedIndex + 1
        | ModifierKeys.None, Key.K -> grid.SelectedIndex <- grid.SelectedIndex - 1
        | _ -> ()
    )

    window.Bookmarks.ItemsSource <- [
        'a', "C:\\animations"
        'b', "C:\\bookmarks"
    ]
    window.BookmarkPanel.Visibility <- Visibility.Visible

    window.Progress.Value <- 0.3
    window.Progress.Visibility <- Visibility.Visible

    window.InputPanel.Visibility <- Visibility.Visible

    window.RegisterPanel.Visibility <- Visibility.Visible

    grid.Focus() |> ignore
    grid.SelectedIndex <- 4

    Application().Run(window) |> ignore
    0
