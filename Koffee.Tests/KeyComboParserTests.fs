module Koffee.KeyComboParserTests

open NUnit.Framework
open FsUnitTyped
open System.Windows.Input

let parseKey keyStr = 
    match KeyComboParser.Parse keyStr with
    | Some keys -> keys
    | None -> failwith (sprintf "Could not parse key string %s" keyStr)

let assertParsesToSingle input expected =
    parseKey input
    |> Seq.exactlyOne
    |> shouldEqual expected

let assertParseSeq charRange startingKey expectedModifer =
    charRange
    |> Seq.mapi (fun i c ->
        let key = enum<Key>((int startingKey) + i)
        (string c, key))
    |> Seq.iter (fun (input, expectedKey) ->
        assertParsesToSingle input (expectedModifer, expectedKey))

[<Test>]
let ``Given a lowercase letter, returns matching key.``() =
    assertParseSeq ['a'..'z'] Key.A ModifierKeys.None

[<Test>]
let ``Given an uppercase letter, returns matching key with shift.``() =
    assertParseSeq ['A'..'Z'] Key.A ModifierKeys.Shift

[<Test>]
let ``Given a number, returns matching key.``() =
    assertParseSeq ['0'..'9'] Key.D0 ModifierKeys.None

[<TestCase("`", Key.Oem3, false)>]
[<TestCase("~", Key.Oem3, true)>]
[<TestCase("!", Key.D1, true)>]
[<TestCase("@", Key.D2, true)>]
[<TestCase("#", Key.D3, true)>]
[<TestCase("$", Key.D4, true)>]
[<TestCase("%", Key.D5, true)>]
[<TestCase("^", Key.D6, true)>]
[<TestCase("&", Key.D7, true)>]
[<TestCase("*", Key.D8, true)>]
[<TestCase("(", Key.D9, true)>]
[<TestCase(")", Key.D0, true)>]
[<TestCase("-", Key.OemMinus, false)>]
[<TestCase("_", Key.OemMinus, true)>]
[<TestCase("=", Key.OemPlus, false)>]
[<TestCase("+", Key.OemPlus, true)>]
[<TestCase("[", Key.OemOpenBrackets, false)>]
[<TestCase("{", Key.OemOpenBrackets, true)>]
[<TestCase("]", Key.Oem6, false)>]
[<TestCase("}", Key.Oem6, true)>]
[<TestCase("\\", Key.Oem5, false)>]
[<TestCase("|", Key.Oem5, true)>]
[<TestCase(";", Key.OemSemicolon, false)>]
[<TestCase(":", Key.OemSemicolon, true)>]
[<TestCase("'", Key.OemQuotes, false)>]
[<TestCase("\"", Key.OemQuotes, true)>]
[<TestCase(",", Key.OemComma, false)>]
[<TestCase(".", Key.OemPeriod, false)>]
[<TestCase("/", Key.OemQuestion, false)>]
[<TestCase("?", Key.OemQuestion, true)>]
let ``Given a symbol, returns matching key.`` input (expectedKey: Key) expectShift =
    let expectedMod = if expectShift then ModifierKeys.Shift else ModifierKeys.None
    assertParsesToSingle input (expectedMod, expectedKey)

[<TestCase("<enter>", Key.Enter)>]
[<TestCase("<tab>", Key.Tab)>]
[<TestCase("<back>", Key.Back)>]
[<TestCase("<f1>", Key.F1)>]
[<TestCase("<scroll>", Key.Scroll)>]
let ``Given a key name, returns matching key.`` input (expectedKey: Key) =
    assertParsesToSingle input (ModifierKeys.None, expectedKey)

[<TestCase("<s>", Key.S, false)>]
[<TestCase("<S>", Key.S, true)>]
[<TestCase("<!>", Key.D1, true)>]
let ``Given a key in brackets, returns matching key and modifier.`` input (expectedKey: Key) expectShift =
    let expectedMod = if expectShift then ModifierKeys.Shift else ModifierKeys.None
    assertParsesToSingle input (expectedMod, expectedKey)

[<TestCase("<s-enter>", Key.Enter)>]
[<TestCase("<S-ENTER>", Key.Enter)>]
[<TestCase("<s-tab>", Key.Tab)>]
[<TestCase("<s-back>", Key.Back)>]
[<TestCase("<s-f1>", Key.F1)>]
let ``Given a key name and shift modifer character, returns matching key with shift.`` input (expectedKey: Key) =
    assertParsesToSingle input (ModifierKeys.Shift, expectedKey)

[<TestCase("a", Key.A, false)>]
[<TestCase("A", Key.A, true)>]
[<TestCase("1", Key.D1, false)>]
[<TestCase("!", Key.D1, true)>]
[<TestCase("enter", Key.Enter, false)>]
let ``Given a modifier character, returns matching key with the modifer.`` inputKey (expectedKey: Key) expectShift =
    [('c', ModifierKeys.Control); ('a', ModifierKeys.Alt); ('m', ModifierKeys.Windows)]
    |> Seq.iter (fun (ch, modifier) ->
        let expectedMod = modifier ||| (if expectShift then ModifierKeys.Shift else ModifierKeys.None)
        let input = sprintf "<%c-%s>" ch inputKey
        assertParsesToSingle input (expectedMod, expectedKey)
    )

[<TestCase("<ca-a>", ModifierKeys.Control ||| ModifierKeys.Alt, Key.A)>]
[<TestCase("<CAM-a>", ModifierKeys.Control ||| ModifierKeys.Alt ||| ModifierKeys.Windows, Key.A)>]
[<TestCase("<sc-enter>", ModifierKeys.Shift ||| ModifierKeys.Control, Key.Enter)>]
let ``Given multiple modifer characters, returns matching key with modifiers.`` input (expectedMod: ModifierKeys) (expectedKey: Key) =
    assertParsesToSingle input (expectedMod, expectedKey)

[<Test>]
let ``Given 3 letters, returns a sequence of those keys.``() =
    parseKey "abc"
    |> shouldEqual [(ModifierKeys.None, Key.A); (ModifierKeys.None, Key.B); (ModifierKeys.None, Key.C)]

[<Test>]
let ``Given 3 chords, returns a sequence of those chords.``() =
    parseKey "<c-a><a-b>C"
    |> shouldEqual [(ModifierKeys.Control, Key.A); (ModifierKeys.Alt, Key.B); (ModifierKeys.Shift, Key.C)]

[<Test>]
let ``Default key bindings, which uses parsing, does not throw an error.``() =
    KeyBinding.defaults |> ignore

[<TestCase("<>")>]
[<TestCase("<s->")>]
[<TestCase("<-tab>")>]
[<TestCase("<s-tab>>")>]
let ``Given invalid input, returns None.`` input =
    KeyComboParser.Parse input
    |> shouldEqual None
