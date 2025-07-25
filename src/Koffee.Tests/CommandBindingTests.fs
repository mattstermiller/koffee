module Koffee.CommandBindingTests

open NUnit.Framework
open FsUnitTyped
open KeyBinder

let createCmdBinding cmd combo1 combo2 : CommandBinding =
    {
        Command = cmd
        KeyCombo1 = combo1
        KeyCombo2 = combo2
    }

let createBinding combo1 combo2 : CommandBinding = createCmdBinding (Cursor CursorUp) combo1 combo2

let noMod = ModifierKeys.None
let comboA : KeyCombo = [noMod, Key.A]
let comboB : KeyCombo = [noMod, Key.B]

[<Test>]
let ``normalize with no combos returns same binding`` () =
    let binding = createBinding [] []
    binding |> CommandBinding.normalize |> shouldEqual binding

[<Test>]
let ``normalize with first combo returns same binding`` () =
    let binding = createBinding comboA []
    binding |> CommandBinding.normalize |> shouldEqual binding

[<Test>]
let ``normalize with different combos returns same binding`` () =
    let binding = createBinding comboA comboB
    binding |> CommandBinding.normalize |> shouldEqual binding

[<Test>]
let ``normalize with same combos returns binding with first combo`` () =
    let binding = createBinding comboA comboA
    binding |> CommandBinding.normalize |> shouldEqual (createBinding comboA [])

[<Test>]
let ``normalize with second combo returns binding with first combo`` () =
    let binding = createBinding [] comboA
    binding |> CommandBinding.normalize |> shouldEqual (createBinding comboA [])


[<Test>]
let ``conflictsWith empty combo returns false`` () =
    createCmdBinding (Cursor CursorUp) comboA []
    |> CommandBinding.conflictsWith (Navigation OpenParent) [] |> shouldEqual false

[<Test>]
let ``conflictsWith same combo but different command space returns false`` () =
    createCmdBinding (Cursor CursorUp) comboA []
    |> CommandBinding.conflictsWith (InputCommand InputHistoryBack) comboA |> shouldEqual false

[<TestCase(false)>]
[<TestCase(true)>]
let ``conflictsWith same combo returns true`` isSecondCombo =
    let binding =
        if isSecondCombo
        then createCmdBinding (Cursor CursorUp) comboB comboA
        else createCmdBinding (Cursor CursorUp) comboA []
    binding |> CommandBinding.conflictsWith (Navigation OpenParent) comboA |> shouldEqual true

[<TestCase(false, false)>]
[<TestCase(false, true)>]
[<TestCase(true, false)>]
[<TestCase(true, true)>]
let ``conflictsWith new prefix returns true`` isSecondCombo isLongCombo =
    let combo = [
        noMod, Key.A
        noMod, Key.B
        if isLongCombo then
            noMod, Key.C
    ]
    let newCombo = combo |> List.take (if isLongCombo then 2 else 1)
    let binding =
        if isSecondCombo
        then createCmdBinding (Cursor CursorUp) comboB  combo
        else createCmdBinding (Cursor CursorUp) combo []
    binding |> CommandBinding.conflictsWith (Navigation OpenParent) newCombo |> shouldEqual true

[<TestCase(false, false)>]
[<TestCase(false, true)>]
[<TestCase(true, false)>]
[<TestCase(true, true)>]
let ``conflictsWith existing prefix returns true`` isSecondCombo isLongCombo =
    let newCombo = [
        noMod, Key.A
        noMod, Key.B
        if isLongCombo then
            noMod, Key.C
    ]
    let combo = newCombo |> List.take (if isLongCombo then 2 else 1)
    let binding =
        if isSecondCombo
        then createCmdBinding (Cursor CursorUp) comboB  combo
        else createCmdBinding (Cursor CursorUp) combo []
    binding |> CommandBinding.conflictsWith (Navigation OpenParent) newCombo |> shouldEqual true

[<TestCase(false, false)>]
[<TestCase(false, true)>]
[<TestCase(true, false)>]
[<TestCase(true, true)>]
let ``conflictsWith different combos returns false`` isModDiff isLongCombo =
    let combo = [
        noMod, Key.A
        if isLongCombo then
            noMod, Key.B
    ]
    let newCombo = [
        if isModDiff then
            ModifierKeys.Shift, Key.A
        else
            noMod, Key.B
        if isLongCombo then
            noMod, Key.B
    ]
    let binding = createCmdBinding (Cursor CursorUp) combo []
    binding |> CommandBinding.conflictsWith (Navigation OpenParent) newCombo |> shouldEqual false


[<Test>]
let ``removeConflictingKeyCombos works as expected`` () =
    let newBinding = createCmdBinding (Cursor CursorUp) comboA comboB
    [
        createCmdBinding (Cursor CursorUp) [noMod, Key.Up] comboA
        createCmdBinding (Cursor CursorDown) comboA [noMod, Key.Down]
        createCmdBinding (Cursor CursorToFirst) [noMod, Key.Left] comboB
        createCmdBinding (Cursor CursorToLast) comboB [noMod, Key.Right]
        createCmdBinding (Cursor CursorUpHalfPage) comboA comboB
        createCmdBinding (Cursor CursorDownHalfPage) comboB comboA
        createCmdBinding (InputCommand InputHistoryBack) comboA comboB
    ]
    |> List.map (CommandBinding.removeConflictingKeyCombos newBinding)
    |> shouldEqual [
        createCmdBinding (Cursor CursorUp) [noMod, Key.Up] []
        createCmdBinding (Cursor CursorDown) [noMod, Key.Down] []
        createCmdBinding (Cursor CursorToFirst) [noMod, Key.Left] []
        createCmdBinding (Cursor CursorToLast) [noMod, Key.Right] []
        createCmdBinding (Cursor CursorUpHalfPage) [] []
        createCmdBinding (Cursor CursorDownHalfPage) [] []
        createCmdBinding (InputCommand InputHistoryBack) comboA comboB
    ]
