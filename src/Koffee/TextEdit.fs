module Koffee.TextEdit

open System.Windows
open System.Windows.Input
open UIHelpers

type Dialog(parent: Window) =
    member _.Open (title: string) (header: string) (text: string) =
        let win = KoffeeUI.TextEditWindow(Owner = parent)
        win.PreviewKeyDown.Add (onKey Key.Escape win.Close)
        win.PreviewKeyDown.Add (onKeyCombo ModifierKeys.Control Key.W win.Close)
        win.Title <- title
        win.Header.Content <- header
        win.TextBox.Text <- text
        win.TextBox.Focus() |> ignore
        win.TextBox.Select(text.Length, 0)
        win.ShowDialog() |> ignore
        win.TextBox.Text
