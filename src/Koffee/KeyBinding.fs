namespace Koffee

open System.Windows.Input
open Acadian.FSharp

module Key =
    let isDigit key = key >= Key.D0 && key <= Key.D9
    let isLetter key = key >= Key.A && key <= Key.Z

[<AutoOpen>]
module KeyPatterns =
    let (|DigitKey|_|) (key: Key) =
        if Key.isDigit key
        then Some (int key - int Key.D0)
        else None

type KeyChord = ModifierKeys * Key

module KeyChord =
    let private modifierStrings = [
        ModifierKeys.Control, "c"
        ModifierKeys.Shift, "s"
        ModifierKeys.Alt, "a"
        ModifierKeys.Windows, "m"
    ]

    let isDigit (modifiers: ModifierKeys, key: Key) =
        modifiers = ModifierKeys.None && Key.isDigit key

    let isLetter (modifiers: ModifierKeys, key: Key) =
        (modifiers = ModifierKeys.None || modifiers = ModifierKeys.Shift) && Key.isLetter key

    let displayString (modifiers: ModifierKeys, key: Key) =
        let modIsOnlyShift = modifiers = ModifierKeys.Shift
        let keyStr =
            match key, modIsOnlyShift with
            | key, false when Key.isLetter key -> (string key).ToLower()
            | key, true when Key.isLetter key -> string key
            | DigitKey digit, false -> string digit
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
            | Key.Back, _ -> "Backspace"
            | Key.Next, _ -> "PageDown"
            | _ -> string key
        let mods =
            if modIsOnlyShift && keyStr.Length = 1 then
                []
            else
                modifierStrings |> List.choose (fun (modifier, str) ->
                    if modifiers.HasFlag modifier then Some str else None
                )
        if not mods.IsEmpty then
            sprintf "<%s-%s>" (mods |> String.concat "") keyStr
        else if keyStr.Length > 1 then
            sprintf "<%s>" keyStr
        else
            keyStr

    let serialize (modifiers: ModifierKeys, key: Key) =
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

    let deserialize str =
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

type KeyCombo = KeyChord list

module KeyCombo =
    let displayString keyCombo =
        keyCombo
        |> List.map KeyChord.displayString
        |> String.concat ""

    let rec intersectsWith (a: KeyCombo) (b: KeyCombo) =
        match a, b with
        | a :: restA, b :: restB when a = b -> intersectsWith restA restB
        | [], _ | _, [] -> true
        | _ -> false

    let isUsableInInputBox keyCombo =
        match keyCombo with
        | [chord] when not (chord |> KeyChord.isLetter) -> true
        | _ -> false

type KeyBindMatch<'a> =
    | Match of 'a
    | PartialMatch
    | NoMatch

type KeyBinding<'Command> = {
    KeyCombo: KeyCombo
    Command: 'Command
}

module KeyBinding =
    let ofTuple (keyCombo, command) =
        { KeyCombo = keyCombo; Command = command }

    let getKeyCombos (bindings: KeyBinding<_> list) command =
        bindings |> List.choose (fun binding ->
            if binding.Command = command
            then Some binding.KeyCombo
            else None
        )

    let getMatch (bindings: KeyBinding<_> seq) (keyCombo: KeyCombo) =
        let rec startsWith prefix lst =
            match prefix, lst with
            | p :: restP, l :: restL when p = l -> startsWith restP restL
            | [], rest -> Some rest
            | _ -> None
        // choose bindings where the next chord matches, selecting the remaining chords
        let matches =
            bindings
            |> Seq.choose (fun binding ->
                startsWith keyCombo binding.KeyCombo |> Option.map (fun restChords -> (restChords, binding.Command))
            )
            |> Seq.toList
        if matches |> List.isEmpty then
            NoMatch
        else
            // see if a binding had all chords matched
            let triggered = matches |> List.tryFind (fst >> (=) [])
            match triggered with
            | Some (_, item) -> Match item
            | None -> PartialMatch

    let getChordMatch (bindings: KeyBinding<_> list) (keyChord: KeyChord) =
        bindings |> List.tryPick (fun binding ->
            if binding.KeyCombo = [keyChord]
            then Some binding.Command
            else None
        )
