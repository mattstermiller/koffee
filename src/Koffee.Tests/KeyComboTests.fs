module Koffee.KeyComboTests

open NUnit.Framework
open FsUnitTyped

[<TestCase(0)>]
[<TestCase(1)>]
[<TestCase(2)>]
let ``intersectsWith empty returns true`` comboLength =
    let combo = List.replicate comboLength (ModifierKeys.None, Key.A)
    combo |> KeyCombo.intersectsWith [] |> shouldEqual true

[<TestCase(false)>]
[<TestCase(true)>]
let ``intersectsWith same combo returns true`` isMultipleKeys =
    let combo = [
        ModifierKeys.None, Key.A
        if isMultipleKeys then
            ModifierKeys.None, Key.B
    ]
    combo |> KeyCombo.intersectsWith [] |> shouldEqual true

[<TestCase(1)>]
[<TestCase(2)>]
let ``intersectsWith other as prefix returns true`` prefixLength =
    let combo = [
        ModifierKeys.None, Key.A
        ModifierKeys.Control ||| ModifierKeys.Shift, Key.B
        ModifierKeys.Control, Key.B
    ]
    let prefix = combo |> List.take prefixLength
    combo |> KeyCombo.intersectsWith prefix |> shouldEqual true

[<TestCase(1)>]
[<TestCase(2)>]
let ``intersectsWith this as prefix returns true`` prefixLength =
    let combo = [
        ModifierKeys.None, Key.A
        ModifierKeys.Control ||| ModifierKeys.Shift, Key.B
        ModifierKeys.Control, Key.B
    ]
    let prefix = combo |> List.take prefixLength
    prefix |> KeyCombo.intersectsWith combo |> shouldEqual true

[<Test>]
let ``intersectsWith different key returns false`` () =
    let combo = [ModifierKeys.None, Key.A]
    let other = [ModifierKeys.None, Key.B]
    combo |> KeyCombo.intersectsWith other |> shouldEqual false

[<Test>]
let ``intersectsWith different modifier returns false`` () =
    let combo = [ModifierKeys.None, Key.A]
    let other = [ModifierKeys.Shift, Key.A]
    combo |> KeyCombo.intersectsWith other |> shouldEqual false

[<Test>]
let ``intersectsWith common prefix returns false`` () =
    let combo = [
        ModifierKeys.None, Key.A
        ModifierKeys.None, Key.B
    ]
    let other = [
        ModifierKeys.None, Key.A
        ModifierKeys.None, Key.C
    ]
    combo |> KeyCombo.intersectsWith other |> shouldEqual false
