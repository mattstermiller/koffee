module Koffee.KeyBinding

open System.Windows.Input
open Koffee

let noMod = ModifierKeys.None
let shift = ModifierKeys.Shift
let ctrl = ModifierKeys.Control
let alt = ModifierKeys.Alt

let defaults: (KeyCombo * MainCommand) list = [
    ([noMod, Key.K], Cursor CursorUp)
    ([noMod, Key.J], Cursor CursorDown)
    ([ctrl, Key.K], Cursor CursorUpHalfPage)
    ([ctrl, Key.U], Cursor CursorUpHalfPage)
    ([ctrl, Key.J], Cursor CursorDownHalfPage)
    ([ctrl, Key.D], Cursor CursorDownHalfPage)
    ([noMod, Key.G; noMod, Key.G], Cursor CursorToFirst)
    ([shift, Key.G], Cursor CursorToLast)
    ([noMod, Key.Space], Cursor SelectToggle)
    ([shift, Key.Space], Cursor SelectRange)
    ([ctrl, Key.A], Cursor SelectAll)
    ([noMod, Key.Z; noMod, Key.T], Cursor (Scroll CursorTop))
    ([noMod, Key.Z; noMod, Key.Z], Cursor (Scroll CursorMiddle))
    ([noMod, Key.Z; noMod, Key.B], Cursor (Scroll CursorBottom))
    ([noMod, Key.F], Cursor (StartFind false))
    ([shift, Key.F], Cursor (StartFind true))
    ([noMod, Key.Oem1], Cursor FindNext)

    ([noMod, Key.L], Navigation OpenCursorItem)
    ([noMod, Key.Enter], Navigation OpenSelected)
    ([shift, Key.Enter], Navigation OpenFileWith)
    ([ctrl, Key.Enter], Navigation OpenFileAndExit)
    ([alt, Key.Enter], Navigation OpenProperties)
    ([ctrl, Key.E], Navigation OpenWithTextEditor)
    ([ctrl ||| shift, Key.T], Navigation OpenTerminal)
    ([ctrl ||| shift, Key.E], Navigation OpenExplorer)
    ([noMod, Key.H], Navigation OpenParent)
    ([noMod, Key.G; noMod, Key.R], Navigation OpenRoot)
    ([noMod, Key.G; noMod, Key.D], Navigation OpenDefault)
    ([shift, Key.H], Navigation Back)
    ([shift, Key.L], Navigation Forward)
    ([noMod, Key.R], Navigation Refresh)
    ([noMod, Key.F5], Navigation Refresh)
    ([noMod, Key.OemQuestion], Navigation StartSearch)
    ([noMod, Key.N], Navigation RepeatPreviousSearch)
    ([noMod, Key.OemQuotes], Navigation (PromptGoToMark Bookmark))
    ([noMod, Key.Oem3], Navigation (PromptGoToMark SavedSearch))
    ([noMod, Key.M], Navigation PromptSetMark)
    ([noMod, Key.S; noMod, Key.N], Navigation (SortList Name))
    ([noMod, Key.S; noMod, Key.M], Navigation (SortList Modified))
    ([noMod, Key.S; noMod, Key.S], Navigation (SortList Size))
    ([noMod, Key.F9], Navigation ToggleHidden)
    ([noMod, Key.G; noMod, Key.H], Navigation ShowNavHistory)
    ([noMod, Key.G; noMod, Key.U], Navigation ShowUndoHistory)
    ([noMod, Key.G; noMod, Key.S], Navigation ShowStatusHistory)

    ([noMod, Key.O], ItemAction CreateFile)
    ([shift, Key.O], ItemAction CreateFolder)
    ([noMod, Key.I], ItemAction (StartRename Begin))
    ([noMod, Key.A], ItemAction (StartRename EndName))
    ([shift, Key.A], ItemAction (StartRename End))
    ([noMod, Key.C], ItemAction (StartRename ReplaceName))
    ([shift, Key.C], ItemAction (StartRename ReplaceAll))
    ([noMod, Key.D], ItemAction (Yank Move))
    ([noMod, Key.Y], ItemAction (Yank Copy))
    ([shift, Key.Y], ItemAction (Yank Shortcut))
    ([alt, Key.Y], ItemAction ClearYank)
    ([noMod, Key.P], ItemAction Put)
    ([noMod, Key.Delete], ItemAction Recycle)
    ([shift, Key.Delete], ItemAction ConfirmDelete)
    ([ctrl, Key.X], ItemAction ClipboardCut)
    ([ctrl, Key.C], ItemAction ClipboardCopy)
    ([ctrl ||| shift, Key.C], ItemAction ClipboardCopyPaths)
    ([ctrl, Key.V], ItemAction ClipboardPaste)
    ([noMod, Key.U], ItemAction Undo)
    ([ctrl, Key.Z], ItemAction Undo)
    ([noMod, Key.U], ItemAction Redo)
    ([ctrl ||| shift, Key.Z], ItemAction Redo)

    ([ctrl, Key.N], Window OpenSplitScreenWindow)
    ([shift, Key.OemQuestion], Window OpenSettings)
    ([noMod, Key.F1], Window OpenSettings)
    ([ctrl, Key.W], Window Exit)
]

type KeyBindMatch<'a> =
    | Match of 'a
    | PartialMatch
    | NoMatch

let getMatch (keyBindings: (KeyCombo * _) list) (keyCombo: KeyCombo) =
    // choose bindings where the next key/chord matches, selecting the remaining chords
    let rec startsWith l sw =
        match l, sw with
        | x :: l, y :: sw when x = y -> startsWith l sw
        | l, [] -> Some l
        | _ -> None
    let matches =
        keyBindings |> List.choose (fun (kc, item) ->
            startsWith kc keyCombo |> Option.map (fun rem -> (rem, item)))
    match matches with
    | [] -> NoMatch
    | _ ->
        // find last binding that had all chords matched
        let triggered = matches |> List.tryFindBack (fst >> (=) [])
        match triggered with
        | Some (_, item) -> Match item
        | None -> PartialMatch

let keyDescription (modifiers: ModifierKeys, key: Key) =
    let isLetter = key >= Key.A && key <= Key.Z
    let mutable showShift = false
    let keyStr =
        match key, modifiers.HasFlag ModifierKeys.Shift with
        | key, false when isLetter -> (string key).ToLower()
        | key, true when isLetter -> string key
        | key, false when key >= Key.D0 && key <= Key.D9 -> (string key).Substring(1)
        | Key.D1, true -> "!"
        | Key.D2, true -> "@"
        | Key.D3, true -> "#"
        | Key.D4, true -> "$"
        | Key.D5, true -> "%"
        | Key.D6, true -> "^"
        | Key.D7, true -> "&"
        | Key.D8, true -> "*"
        | Key.D9, true -> "("
        | Key.D0, true -> ")"
        | Key.Oem3, false -> "`"
        | Key.Oem3, true -> "~"
        | Key.OemMinus, false -> "-"
        | Key.OemMinus, true -> "_"
        | Key.OemPlus, false -> "="
        | Key.OemPlus, true -> "+"
        | Key.OemOpenBrackets, false -> "["
        | Key.OemOpenBrackets, true -> "{"
        | Key.Oem6, false -> "]"
        | Key.Oem6, true -> "}"
        | Key.Oem5, false -> "\\"
        | Key.Oem5, true -> "|"
        | Key.OemSemicolon, false -> ";"
        | Key.OemSemicolon, true -> ":"
        | Key.OemQuotes, false -> "'"
        | Key.OemQuotes, true -> "\""
        | Key.OemComma, false -> ","
        | Key.OemComma, true -> "<"
        | Key.OemPeriod, false -> "."
        | Key.OemPeriod, true -> ">"
        | Key.OemQuestion, false -> "/"
        | Key.OemQuestion, true -> "?"
        | Key.Return, shift ->
            showShift <- shift
            "Enter"
        | _, shift ->
            showShift <- shift
            string key
    let mods =
        [ ModifierKeys.Control, "c"
          ModifierKeys.Shift, "s"
          ModifierKeys.Alt, "a"
          ModifierKeys.Windows, "m"
        ]
        |> List.filter (fun (m, _) -> m <> ModifierKeys.Shift || showShift)
        |> List.choose (fun (m, s) -> if modifiers.HasFlag m then Some s else None)
    if mods.IsEmpty then
        keyStr
    else
        sprintf "<%s-%s>" (mods |> String.concat "") keyStr

let getKeyCombo evt = defaults |> List.find (snd >> (=) evt) |> fst

let getKeyComboDescription evt = getKeyCombo evt |> List.map keyDescription |> String.concat ""
