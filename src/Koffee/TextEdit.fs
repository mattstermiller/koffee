module Koffee.TextEdit

open System.Windows.Input
open UIHelpers

let showDialog (title: string) (header: string) (text: string) =
    let win = KoffeeUI.TextEditWindow()
    win.PreviewKeyDown.Add (onKey Key.Escape win.Close)
    win.PreviewKeyDown.Add (onKeyCombo ModifierKeys.Control Key.W win.Close)
    win.Title <- title
    win.Header.Content <- header
    win.TextBox.Text <- text
    win.TextBox.Focus() |> ignore
    win.TextBox.Select(text.Length, 0)
    win.ShowDialog() |> ignore
    win.TextBox.Text
