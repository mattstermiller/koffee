module Koffee.MainLogicTests_FindSearch

open NUnit.Framework
open FsUnitTyped

let items names =
    let toItem name = Item.Basic Path.Root name Folder
    names |> Seq.map toItem |> Seq.toList

let testModel cursorStart =
    { testModel with
        Items = "alice,bob,charlie,crystal,apple,cherry".Split(',') |> items
        Cursor = cursorStart
    }

let assertEqualExceptCursor expected actual =
    assertAreEqualWith expected actual (ignoreMembers ["Cursor"; "Items"; "InputText"])

let doFind next nth prefix cursorStart =
    let model =
        { testModel cursorStart with
            LastFind = Some prefix
            KeyComboCount = nth
        }

    let actual =
        if next then
            MainLogic.Search.findNext model
        else
            MainLogic.Search.find prefix model

    let expected =
        { model with
            LastFind = Some prefix
            Status = if next then Some (MainStatus.find prefix nth) else None
        }
    assertEqualExceptCursor expected actual
    actual.Cursor

let find = doFind false None
let findNext = doFind true None
let findNextNth count = doFind true <| Some count

[<Test>]
let ``Find that matches nothing should not change the cursor``() =
    find "d" 1 |> shouldEqual 1

[<Test>]
let ``Find that matches only the current item should not change the cursor``() =
    find "b" 1 |> shouldEqual 1

[<Test>]
let ``Find that matches the current and next item should not change the cursor``() =
    find "c" 2 |> shouldEqual 2

[<Test>]
let ``Find that matches a item wrapping around should set the cursor to the that index``() =
    find "b" 2 |> shouldEqual 1

[<Test>]
let ``Find next that matches nothing should not change the cursor``() =
    findNext "d" 1 |> shouldEqual 1

[<Test>]
let ``Find next that matches only the current item should not change the cursor``() =
    findNext "b" 1 |> shouldEqual 1

[<Test>]
let ``Find next that matches the current and next item should set the cursor to the next index``() =
    findNext "c" 2 |> shouldEqual 3

[<Test>]
let ``Find next that matches a item wrapping around should set the cursor to the that index``() =
    findNext "b" 2 |> shouldEqual 1

[<TestCase(1, 2)>]
[<TestCase(2, 3)>]
[<TestCase(3, 5)>]
let ``Find next nth that matches`` nth expected =
    findNextNth nth "c" 1 |> shouldEqual expected
