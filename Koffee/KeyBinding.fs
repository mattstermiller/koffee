module Koffee.KeyBinding

open System.Windows.Input

let private key keyStr = 
    match KeyComboParser.Parse keyStr with
    | Some keys -> keys
    | None -> failwith (sprintf "Could not parse key string %s for binding" keyStr)

let Defaults = [
    (key "j", CursorDown)
    (key "<c-j>", CursorDownHalfPage)
    (key "<c-d>", CursorDownHalfPage)
    (key "k", CursorUp)
    (key "<c-k>", CursorUpHalfPage)
    (key "<c-u>", CursorUpHalfPage)
    (key "gg", CursorToFirst)
    (key "G", CursorToLast)
    (key "h", OpenParent)
    (key "l", OpenSelected)
    (key "<enter>", OpenSelected)
    (key "f", StartInput FindInput)
    (key ";", FindNext)
    (key "/", StartInput SearchInput)
    (key "n", SearchNext)
    (key "<c-e>", OpenExplorer)
    (key "<f9>", TogglePathFormat)
]

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
