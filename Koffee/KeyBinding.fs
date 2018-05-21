module Koffee.KeyBinding

let private parseKey keyStr = 
    match KeyComboParser.Parse keyStr with
    | Some keys -> keys
    | None -> failwith (sprintf "Could not parse key string %s for binding" keyStr)

let defaultsAsString = [
    ("k", CursorUp)
    ("j", CursorDown)
    ("<c-k>", CursorUpHalfPage)
    ("<c-u>", CursorUpHalfPage)
    ("<c-j>", CursorDownHalfPage)
    ("<c-d>", CursorDownHalfPage)
    ("gg", CursorToFirst)
    ("G", CursorToLast)
    ("l", OpenSelected)
    ("<enter>", OpenSelected)
    ("h", OpenParent)
    ("H", Back)
    ("L", Forward)
    ("r", Refresh)
    ("<f5>", Refresh)
    ("u", Undo)
    ("<c-z>", Undo)
    ("U", Redo)
    ("<cs-z>", Redo)
    ("o", StartInput CreateFile)
    ("O", StartInput CreateFolder)
    ("i", StartInput (Rename Begin))
    ("a", StartInput (Rename EndName))
    ("A", StartInput (Rename End))
    ("c", StartInput (Rename ReplaceName))
    ("C", StartInput (Rename ReplaceAll))
    ("f", StartPrompt (Find false))
    ("F", StartPrompt (Find true))
    (";", FindNext)
    ("/", StartInput Search)
    ("n", SearchNext)
    ("N", SearchPrevious)
    ("'", StartPrompt GoToBookmark)
    ("m", StartPrompt SetBookmark)
    ("d", StartMove)
    ("y", StartCopy)
    ("p", Put)
    ("<delete>", Recycle)
    ("<s-delete>", StartConfirm Delete)
    ("sn", SortList Name)
    ("sm", SortList Modified)
    ("ss", SortList Size)
    ("<f9>", ToggleHidden)
    ("<c-n>", OpenSplitScreenWindow)
    ("<cs-e>", OpenExplorer)
    ("<cs-c>", OpenCommandLine)
    ("<cs-t>", OpenWithTextEditor)
    ("?", OpenSettings)
    ("<f1>", OpenSettings)
    ("<c-w>", Exit)
]

let defaults =
    defaultsAsString
    |> List.map (fun (keyStr, evt) -> (parseKey keyStr, evt))

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
        let triggered = matches |> List.tryFindBack (fun (keyCombo, _) -> keyCombo = [])
        match triggered with
        | Some (_, item) -> Match item
        | None -> PartialMatch
