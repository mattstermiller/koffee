namespace Koffee

open System.Windows.Input

type KeyCombo = (ModifierKeys * Key) list

module KeyComboParser =
    let private (|ParseKey|_|) char =
        let keyFromOffset startKeyValue startCharValue ch =
            let offset = (int ch) - (int startCharValue)
            enum<Key>((int startKeyValue) + offset)
        match char with
        | c when c >= 'a' && c <= 'z' -> Some (ModifierKeys.None, keyFromOffset Key.A 'a' c)
        | c when c >= 'A' && c <= 'Z' -> Some (ModifierKeys.Shift, keyFromOffset Key.A 'A' c)
        | c when c >= '0' && c <= '9' -> Some (ModifierKeys.None, keyFromOffset Key.D0 '0' c)
        | c when c = '!' -> Some (ModifierKeys.Shift, Key.D1)
        | c when c = '@' -> Some (ModifierKeys.Shift, Key.D2)
        | c when c = '#' -> Some (ModifierKeys.Shift, Key.D3)
        | c when c = '$' -> Some (ModifierKeys.Shift, Key.D4)
        | c when c = '%' -> Some (ModifierKeys.Shift, Key.D5)
        | c when c = '^' -> Some (ModifierKeys.Shift, Key.D6)
        | c when c = '&' -> Some (ModifierKeys.Shift, Key.D7)
        | c when c = '*' -> Some (ModifierKeys.Shift, Key.D8)
        | c when c = '(' -> Some (ModifierKeys.Shift, Key.D9)
        | c when c = ')' -> Some (ModifierKeys.Shift, Key.D0)
        | c when c = '`' -> Some (ModifierKeys.None, Key.Oem3)
        | c when c = '~' -> Some (ModifierKeys.Shift, Key.Oem3)
        | c when c = '-' -> Some (ModifierKeys.None, Key.OemMinus)
        | c when c = '_' -> Some (ModifierKeys.Shift, Key.OemMinus)
        | c when c = '=' -> Some (ModifierKeys.None, Key.OemPlus)
        | c when c = '+' -> Some (ModifierKeys.Shift, Key.OemPlus)
        | c when c = '[' -> Some (ModifierKeys.None, Key.OemOpenBrackets)
        | c when c = '{' -> Some (ModifierKeys.Shift, Key.OemOpenBrackets)
        | c when c = ']' -> Some (ModifierKeys.None, Key.Oem6)
        | c when c = '}' -> Some (ModifierKeys.Shift, Key.Oem6)
        | c when c = '\\' -> Some (ModifierKeys.None, Key.Oem5)
        | c when c = '|' -> Some (ModifierKeys.Shift, Key.Oem5)
        | c when c = ';' -> Some (ModifierKeys.None, Key.OemSemicolon)
        | c when c = ':' -> Some (ModifierKeys.Shift, Key.OemSemicolon)
        | c when c = ''' -> Some (ModifierKeys.None, Key.OemQuotes)
        | c when c = '\"' -> Some (ModifierKeys.Shift, Key.OemQuotes)
        | c when c = ',' -> Some (ModifierKeys.None, Key.OemComma)
        | c when c = '.' -> Some (ModifierKeys.None, Key.OemPeriod)
        | c when c = '/' -> Some (ModifierKeys.None, Key.OemQuestion)
        | c when c = '?' -> Some (ModifierKeys.Shift, Key.OemQuestion)
        | _ -> None

    let private (|ParseKeyName|_|) chars =
        let keyFromCharSeq nameChars =
            let name = nameChars |> Array.ofList |> System.String
            match System.Enum.TryParse<Key>(name, true) with
            | (true, key) -> Some key
            | _ -> None
        let rec readKeyName chars acc =
            match chars with
            | ParseKey combo :: ('>' :: rest) when acc = [] -> Some (combo, rest)
            | '>' :: rest ->
                keyFromCharSeq(List.rev acc)
                |> Option.map (fun key -> ((ModifierKeys.None, key), rest))
            | c :: rest when System.Char.IsLetterOrDigit c -> readKeyName rest (c :: acc)
            | _ -> None
        readKeyName chars []

    let private (|ParseMod|_|) char =
        match System.Char.ToLower(char) with
        | 's' -> Some ModifierKeys.Shift
        | 'c' -> Some ModifierKeys.Control
        | 'a' -> Some ModifierKeys.Alt
        | 'm' -> Some ModifierKeys.Windows
        | _ -> None

    let private (|ParseChord|_|) chars =
        let rec readModifier chars modifier =
            match chars with
            | '-' :: rest when modifier <> ModifierKeys.None -> Some (modifier, rest)
            | ParseMod m :: rest -> readModifier rest (modifier ||| m)
            | _ -> None
        readModifier chars ModifierKeys.None
        |> Option.bind
            (fun (modifier, chars) ->
                match chars with
                | ParseKeyName ((keyMod, key), rest) -> Some ((modifier ||| keyMod, key), rest)
                | _ -> None)

    let rec private ParseBody chars keys =
        match chars with
        | '<' :: charsAfterOpen ->
            match charsAfterOpen with
            | ParseChord (combo, charsAfterClose) -> ParseBody charsAfterClose (combo :: keys)
            | ParseKeyName (combo, charsAfterClose) -> ParseBody charsAfterClose (combo :: keys)
            | _ -> None
        | ParseKey combo :: rest -> ParseBody rest (combo :: keys)
        | [] -> Some (List.rev keys)
        | _ -> None

    let Parse bindStr : KeyCombo option =
        let chars = bindStr |> List.ofSeq
        ParseBody chars []
