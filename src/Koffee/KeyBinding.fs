module Koffee.KeyBinding

let defaultsAsString = [
    ("k", Cursor CursorUp)
    ("j", Cursor CursorDown)
    ("<c-k>", Cursor CursorUpHalfPage)
    ("<c-u>", Cursor CursorUpHalfPage)
    ("<c-j>", Cursor CursorDownHalfPage)
    ("<c-d>", Cursor CursorDownHalfPage)
    ("gg", Cursor CursorToFirst)
    ("G", Cursor CursorToLast)
    ("<space>", Cursor SelectToggle)
    ("<s-space>", Cursor SelectRange)
    ("<c-a>", Cursor SelectAll)
    ("zt", Cursor (Scroll CursorTop))
    ("zz", Cursor (Scroll CursorMiddle))
    ("zb", Cursor (Scroll CursorBottom))
    ("f", Cursor (StartFind false))
    ("F", Cursor (StartFind true))
    (";", Cursor FindNext)

    ("h", Navigation OpenParent)
    ("gr", Navigation OpenRoot)
    ("gd", Navigation OpenDefault)
    ("H", Navigation Back)
    ("L", Navigation Forward)
    ("r", Navigation Refresh)
    ("<f5>", Navigation Refresh)
    ("/", Navigation StartSearch)
    ("n", Navigation RepeatPreviousSearch)
    ("'", Navigation PromptGoToBookmark)
    ("m", Navigation PromptSetBookmarkOrSavedSearch)
    ("`", Navigation PromptGoToSavedSearch)
    ("sn", Navigation (SortList Name))
    ("sm", Navigation (SortList Modified))
    ("ss", Navigation (SortList Size))
    ("<f9>", Navigation ToggleHidden)
    ("gh", Navigation ShowNavHistory)
    ("gu", Navigation ShowUndoHistory)
    ("gs", Navigation ShowStatusHistory)

    ("l", ItemAction OpenCursorItem)
    ("<enter>", ItemAction OpenSelected)
    ("<s-enter>", ItemAction OpenFileWith)
    ("<c-enter>", ItemAction OpenFileAndExit)
    ("<a-enter>", ItemAction OpenProperties)
    ("<c-e>", ItemAction OpenWithTextEditor)
    ("<cs-t>", ItemAction OpenTerminal)
    ("<cs-e>", ItemAction OpenExplorer)
    ("o", ItemAction CreateFile)
    ("O", ItemAction CreateFolder)
    ("i", ItemAction (StartRename Begin))
    ("a", ItemAction (StartRename EndName))
    ("A", ItemAction (StartRename End))
    ("c", ItemAction (StartRename ReplaceName))
    ("C", ItemAction (StartRename ReplaceAll))
    ("d", ItemAction (Yank Move))
    ("y", ItemAction (Yank Copy))
    ("Y", ItemAction (Yank Shortcut))
    ("<a-y>", ItemAction ClearYank)
    ("p", ItemAction Put)
    ("<delete>", ItemAction Recycle)
    ("<s-delete>", ItemAction ConfirmDelete)
    ("<c-x>", ItemAction ClipboardCut)
    ("<c-c>", ItemAction ClipboardCopy)
    ("<cs-c>", ItemAction ClipboardCopyPaths)
    ("<c-v>", ItemAction ClipboardPaste)
    ("u", ItemAction Undo)
    ("<c-z>", ItemAction Undo)
    ("U", ItemAction Redo)
    ("<cs-z>", ItemAction Redo)

    ("<c-n>", Window OpenSplitScreenWindow)
    ("?", Window OpenSettings)
    ("<f1>", Window OpenSettings)
    ("<c-w>", Window Exit)
]

let private parseKey keyStr =
    match KeyComboParser.Parse keyStr with
    | Some keys -> keys
    | None -> failwith (sprintf "Could not parse key string %s for binding" keyStr)

let defaults =
    defaultsAsString
    |> List.map (fun (keyStr, evt) -> (parseKey keyStr, evt))

let getKeysString evt = defaultsAsString |> List.find (snd >> (=) evt) |> fst

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

open System.Windows.Input

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
