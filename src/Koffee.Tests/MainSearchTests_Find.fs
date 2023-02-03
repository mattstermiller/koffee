module Koffee.MainSearchTests_Find

open NUnit.Framework
open FsUnitTyped
open Acadian.FSharp
open Koffee.Main

let items names =
    let toItem name = Item.Basic Path.Root name Folder
    names |> Seq.map toItem |> Seq.toList

let doFind next repeat findText cursorStart =
    let model =
        { testModel with
            Items = "alice,bob,charlie,crystal,apple,cherry".Split(',') |> items
            Cursor = cursorStart
            RepeatCommand = repeat
            InputError = Some (FindFailure "old error")
            InputText = if not next then findText else ""
            LastFind = if next then Some findText else None
        }

    let actual =
        if next then
            Search.findNext model
        else
            Search.find model

    let expected =
        { model with
            LastFind = findText |> Option.ofString
            Status = if next then Some (MainStatus.Message (MainStatus.Find (findText, (repeat |? 1)))) else None
        }
    assertAreEqualWith expected actual (ignoreMembers ["Cursor"; "InputError"])
    (actual.Cursor, actual.InputError)

let find = doFind false None
let findNext = doFind true None
let findNextNth count = doFind true (Some count)

[<Test>]
let ``Find empty string clears error``() =
    find "" 0 |> shouldEqual (0, None)

[<Test>]
let ``Find that matches nothing should show error and not change the cursor``() =
    find "d" 1 |> shouldEqual (1, Some (FindFailure "d"))

[<Test>]
let ``Find that matches only the current item should not change the cursor``() =
    find "b" 1 |> shouldEqual (1, None)

[<Test>]
let ``Find that matches the current and next item should not change the cursor``() =
    find "c" 2 |> shouldEqual (2, None)

[<Test>]
let ``Find that matches an item wrapping around should set the cursor to the that index``() =
    find "b" 2 |> shouldEqual (1, None)

[<Test>]
let ``Find next that matches nothing should show error and not change the cursor``() =
    findNext "d" 1 |> shouldEqual (1, Some (FindFailure "d"))

[<Test>]
let ``Find next that matches only the current item should not change the cursor``() =
    findNext "b" 1 |> shouldEqual (1, None)

[<Test>]
let ``Find next that matches the current and next item should set the cursor to the next index``() =
    findNext "c" 2 |> shouldEqual (3, None)

[<Test>]
let ``Find next that matches an item wrapping around should set the cursor to the that index``() =
    findNext "b" 2 |> shouldEqual (1, None)

[<TestCase(1, 2)>]
[<TestCase(2, 3)>]
[<TestCase(3, 5)>]
let ``Find next nth that matches`` nth expected =
    findNextNth nth "c" 1 |> shouldEqual (expected, None)
