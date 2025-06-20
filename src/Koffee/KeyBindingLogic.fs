module Koffee.KeyBindingLogic

open System.Windows.Input
open Koffee
open Acadian.FSharp

type KeyBindMatch<'a> =
    | Match of 'a
    | PartialMatch
    | NoMatch

let getMatch (bindings: KeyBinding list) (keyCombo: KeyCombo) =
    // choose bindings where the next key/chord matches, selecting the remaining chords
    let rec startsWith l sw =
        match l, sw with
        | x :: l, y :: sw when x = y -> startsWith l sw
        | l, [] -> Some l
        | _ -> None
    let matches =
        bindings |> List.choose (fun binding ->
            startsWith binding.KeyCombo keyCombo |> Option.map (fun rest -> (rest, binding.Command))
        )
    match matches with
    | [] -> NoMatch
    | _ ->
        // find last binding that had all chords matched
        let triggered = matches |> List.tryFindBack (fst >> (=) [])
        match triggered with
        | Some (_, item) -> Match item
        | None -> PartialMatch

let getChordMatch (bindings: KeyBinding list) (keyChord: ModifierKeys * Key) =
    bindings |> List.tryPick (fun binding ->
        if binding.KeyCombo = [keyChord]
        then Some binding.Command
        else None
    )

let private modifierStrings = [
    ModifierKeys.Control, "c"
    ModifierKeys.Shift, "s"
    ModifierKeys.Alt, "a"
    ModifierKeys.Windows, "m"
]

let chordDescription (modifiers: ModifierKeys, key: Key) =
    let isLetter = key >= Key.A && key <= Key.Z
    let modIsOnlyShift = modifiers = ModifierKeys.Shift
    let keyStr =
        match key, modIsOnlyShift with
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
        | Key.Return, _ -> "Enter"
        | _ -> string key
    let mods =
        if modIsOnlyShift && keyStr.Length = 1 then
            []
        else
            modifierStrings |> List.choose (fun (modifier, str) ->
                if modifiers.HasFlag modifier then Some str else None
            )
    if mods.IsEmpty then
        keyStr
    else
        sprintf "<%s-%s>" (mods |> String.concat "") keyStr

let keyComboDescription keyCombo =
    keyCombo
    |> List.map chordDescription
    |> String.concat ""

let getKeyCombos (bindings: KeyBinding list) command =
    bindings |> List.choose (fun binding ->
        if binding.Command = command
        then Some binding.KeyCombo
        else None
    )

module Serialization =
    let chordString (modifiers: ModifierKeys, key: Key) =
        let mods =
            [
                ModifierKeys.Control
                ModifierKeys.Shift
                ModifierKeys.Alt
                ModifierKeys.Windows
            ]
            |> List.filter (modifiers.HasFlag)
            |> List.map (fun modifier -> string modifier + "+")
            |> String.concat ""
        mods + string key

    let parseChord str =
        let parts = str |> String.split '+'
        let mods =
            parts
            |> Seq.take (parts.Length - 1)
            |> Seq.choose (Parse.enumValue<ModifierKeys>)
            |> Seq.fold (|||) ModifierKeys.None
        parts
        |> Array.last
        |> Parse.enumValue<Key>
        |> Option.map (fun key -> (mods, key))
