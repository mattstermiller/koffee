module Koffee.KeyBinding

open System.Windows.Input

let noMod = ModifierKeys.None
let shift = ModifierKeys.Shift
let ctrl = ModifierKeys.Control
let alt = ModifierKeys.Alt

let defaults: (KeyCombo * MainCommand) list = [
    ([noMod, Key.K], CursorUp)
    ([noMod, Key.J], CursorDown)
    ([ctrl, Key.K], CursorUpHalfPage)
    ([ctrl, Key.U], CursorUpHalfPage)
    ([ctrl, Key.J], CursorDownHalfPage)
    ([ctrl, Key.D], CursorDownHalfPage)
    ([noMod, Key.G; noMod, Key.G], CursorToFirst)
    ([shift, Key.G], CursorToLast)
    ([noMod, Key.Space], SelectToggle)
    ([shift, Key.Space], SelectRange)
    ([ctrl, Key.A], SelectAll)
    ([noMod, Key.Z; noMod, Key.T], Scroll CursorTop)
    ([noMod, Key.Z; noMod, Key.Z], Scroll CursorMiddle)
    ([noMod, Key.Z; noMod, Key.B], Scroll CursorBottom)
    ([noMod, Key.L], OpenCursorItem)
    ([noMod, Key.Enter], OpenSelected)
    ([shift, Key.Enter], OpenFileWith)
    ([ctrl, Key.Enter], OpenFileAndExit)
    ([alt, Key.Enter], OpenProperties)
    ("h", OpenParent)
    ("gr", OpenRoot)
    ("gd", OpenDefault)
    ("H", Back)
    ("L", Forward)
    ("r", Refresh)
    ("<f5>", Refresh)
    ("u", Undo)
    ("<c-z>", Undo)
    ("U", Redo)
    ("<cs-z>", Redo)
    ("gh", ToggleHistory NavHistory)
    ("gu", ToggleHistory UndoHistory)
    ("gs", ToggleHistory StatusHistory)
    ("o", StartInput CreateFile)
    ("O", StartInput CreateFolder)
    ("i", StartInput (Rename Begin))
    ("a", StartInput (Rename EndName))
    ("A", StartInput (Rename End))
    ("c", StartInput (Rename ReplaceName))
    ("C", StartInput (Rename ReplaceAll))
    ("f", StartInput (Find false))
    ("F", StartInput (Find true))
    (";", FindNext)
    ("/", StartInput Search)
    ("n", RepeatPreviousSearch)
    ("'", StartPrompt GoToBookmark)
    ("m", StartPrompt SetBookmark)
    ("`", StartPrompt GoToSavedSearch)
    ("d", StartPut Move)
    ("y", StartPut Copy)
    ("Y", StartPut Shortcut)
    ("<a-y>", ClearYank)
    ("p", Put)
    ("<c-x>", ClipboardCut)
    ("<c-c>", ClipboardCopy)
    ("<cs-c>", ClipboardCopyPaths)
    ("<c-v>", ClipboardPaste)
    ("<delete>", Recycle)
    ("<s-delete>", ConfirmDelete)
    ("sn", SortList Name)
    ("sm", SortList Modified)
    ("ss", SortList Size)
    ("<f9>", ToggleHidden)
    ("<c-n>", OpenSplitScreenWindow)
    ("<c-e>", OpenWithTextEditor)
    ("<cs-t>", OpenTerminal)
    ("<cs-e>", OpenExplorer)
    ("?", OpenSettings)
    ("<f1>", OpenSettings)
    ("<c-w>", Exit)
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
