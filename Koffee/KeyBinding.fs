module Koffee.KeyBinding

open System.Windows.Input

let private parseKey keyStr = 
    match KeyComboParser.Parse keyStr with
    | Some keys -> keys
    | None -> failwith (sprintf "Could not parse key string %s for binding" keyStr)

let DefaultsAsString = [
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
    ("<f5>", Refresh)
    ("u", Undo)
    ("<c-z>", Undo)
    ("U", Redo)
    ("<cs-z>", Redo)
    ("o", StartInput CreateFile)
    ("O", StartInput CreateFolder)
    ("D", StartInput DeletePermanently)
    ("i", StartInput (Rename Begin))
    ("a", StartInput (Rename End))
    ("c", StartInput (Rename Replace))
    ("f", StartInput Find)
    (";", FindNext)
    ("/", StartInput Search)
    ("n", SearchNext)
    ("N", SearchPrevious)
    ("d", Delete)
    ("<f9>", TogglePathFormat)
    ("?", OpenSettings)
    ("<f1>", OpenSettings)
    ("<c-e>", OpenExplorer)
]

let Defaults =
    DefaultsAsString
    |> List.map (fun (keyStr, evt) -> (parseKey keyStr, evt))

let GetMatch (currBindings: (KeyCombo * 'a) list) (chord: ModifierKeys * Key) =
    // choose bindings where the next key/chord matches, selecting the remaining chords
    let matchBindings =
        currBindings
        |> List.choose
            (fun (keyCombo, item) ->
                match keyCombo with
                | kc :: rest when kc = chord-> Some (rest, item)
                | _ -> None)
    // find bindings that had all chords matched
    let triggered =
        matchBindings
        |> List.filter (fun (keyCombo, item) -> keyCombo = [])
    // if any were triggered, return only the last one; else return all matched bindings
    match triggered with
    | [] -> matchBindings
    | trig -> [List.last trig]
