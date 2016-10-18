module Koffee.KeyBinding

open System.Windows.Input

let private key keyStr = 
    match KeyComboParser.Parse keyStr with
    | Some keys -> keys
    | None -> failwith (sprintf "Could not parse key string %s for binding" keyStr)

let Defaults = [
    (key "j", NavDown)
    (key "<c-j>", NavDownHalfPage)
    (key "<c-d>", NavDownHalfPage)
    (key "k", NavUp)
    (key "<c-k>", NavUpHalfPage)
    (key "<c-u>", NavUpHalfPage)
    (key "gg", NavToFirst)
    (key "G", NavToLast)
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

    let triggered =
        matchBindings
        |> List.filter (fun (keyCombo, item) -> keyCombo = [])

    match triggered with
    | [] -> matchBindings
    | trig -> [List.last trig]
