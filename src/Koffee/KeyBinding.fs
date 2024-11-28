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
    ([noMod, Key.OemSemicolon], Cursor FindNext)

    // ([noMod, Key.L], OpenCursorItem)
    // ([noMod, Key.Enter], OpenSelected)
    // ([shift, Key.Enter], OpenFileWith)
    // ([ctrl, Key.Enter], OpenFileAndExit)
    // ([alt, Key.Enter], OpenProperties)

    // ("l", Navigation OpenCursorItem)
    // ("<enter>", Navigation OpenSelected)
    // ("<s-enter>", Navigation OpenFileWith)
    // ("<c-enter>", Navigation OpenFileAndExit)
    // ("<a-enter>", Navigation OpenProperties)
    // ("<c-e>", Navigation OpenWithTextEditor)
    // ("<cs-t>", Navigation OpenTerminal)
    // ("<cs-e>", Navigation OpenExplorer)
    // ("h", Navigation OpenParent)
    // ("gr", Navigation OpenRoot)
    // ("gd", Navigation OpenDefault)
    // ("H", Navigation Back)
    // ("L", Navigation Forward)
    // ("r", Navigation Refresh)
    // ("<f5>", Navigation Refresh)
    // ("/", Navigation StartSearch)
    // ("n", Navigation RepeatPreviousSearch)
    // ("'", Navigation (PromptGoToMark Bookmark))
    // ("`", Navigation (PromptGoToMark SavedSearch))
    // ("m", Navigation PromptSetMark)
    // ("sn", Navigation (SortList Name))
    // ("sm", Navigation (SortList Modified))
    // ("ss", Navigation (SortList Size))
    // ("<f9>", Navigation ToggleHidden)
    // ("gh", Navigation ShowNavHistory)
    // ("gu", Navigation ShowUndoHistory)
    // ("gs", Navigation ShowStatusHistory)

    // ("o", ItemAction CreateFile)
    // ("O", ItemAction CreateFolder)
    // ("i", ItemAction (StartRename Begin))
    // ("a", ItemAction (StartRename EndName))
    // ("A", ItemAction (StartRename End))
    // ("c", ItemAction (StartRename ReplaceName))
    // ("C", ItemAction (StartRename ReplaceAll))
    // ("d", ItemAction (Yank Move))
    // ("y", ItemAction (Yank Copy))
    // ("Y", ItemAction (Yank Shortcut))
    // ("<a-y>", ItemAction ClearYank)
    // ("p", ItemAction Put)
    // ("<delete>", ItemAction Recycle)
    // ("<s-delete>", ItemAction ConfirmDelete)
    // ("<c-x>", ItemAction ClipboardCut)
    // ("<c-c>", ItemAction ClipboardCopy)
    // ("<cs-c>", ItemAction ClipboardCopyPaths)
    // ("<c-v>", ItemAction ClipboardPaste)
    // ("u", ItemAction Undo)
    // ("<c-z>", ItemAction Undo)
    // ("U", ItemAction Redo)
    // ("<cs-z>", ItemAction Redo)

    // ("<c-n>", Window OpenSplitScreenWindow)
    // ("?", Window OpenSettings)
    // ("<f1>", Window OpenSettings)
    // ("<c-w>", Window Exit)
]

// let private parseKey keyStr =
//     match KeyComboParser.Parse keyStr with
//     | Some keys -> keys
//     | None -> failwith (sprintf "Could not parse key string %s for binding" keyStr)

// let defaults =
//     defaultsAsString
//     |> List.map (fun (keyStr, evt) -> (parseKey keyStr, evt))

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
