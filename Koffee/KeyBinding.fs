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
    (key "f", StartInput FindInput)
    (key "<c-e>", OpenExplorer)
]
