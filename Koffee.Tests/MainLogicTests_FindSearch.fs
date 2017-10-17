module Koffee.MainLogicTests_FindSearch

open NUnit.Framework
open FsUnitTyped
open Testing
open KellermanSoftware.CompareNetObjects

let nodes names =
    let toNode name = {
        Name = name
        Path = Path.Root
        Type = Folder
        Modified = None
        Size = None
        IsHidden = false
        IsSearchMatch = false
    }
    names |> Seq.map toNode |> Seq.toList

let createModel cursorStart =
    let model = createBaseTestModel()
    model.Nodes <- "alice,bob,charlie,crystal,apple,cherry".Split(',') |> nodes
    model.Cursor <- cursorStart
    model

let assertEqualExceptCursor expected actual =
    CompareLogic()
    |> ignoreMembers ["Cursor"; "Nodes"; "InputText"]
    |> assertAreEqualWith expected actual

let find char cursorStart =
    let model = createModel cursorStart
    let expected = createModel cursorStart

    MainLogic.Cursor.find char model

    expected.LastFind <- (Some char)
    expected.Status <- Some <| MainStatus.find char
    assertEqualExceptCursor expected model
    model.Cursor

[<Test>]
let ``Find a char that matches nothing should not change the cursor``() =
    find 'A' 1 |> shouldEqual 1

[<Test>]
let ``Find a char that matches only the current node should not change the cursor``() =
    find 'b' 1 |> shouldEqual 1

[<Test>]
let ``Find a char that matches the current and next node should set the cursor to the next index``() =
    find 'c' 2 |> shouldEqual 3

[<Test>]
let ``Find a char that matches a node wrapping around should set the cursor to the that index``() =
    find 'b' 2 |> shouldEqual 1


type SearchResult = {
    Cursor: int
    Count: int
}
with
    static member fromModel (model: MainModel) =
        { Cursor = model.Cursor
          Count = model.Nodes |> Seq.filter (fun n -> n.IsSearchMatch) |> Seq.length }

let assertSearchGiven reverse searchStr cursorStart expectedResult =
    let search = if searchStr <> "" then Some searchStr else None
    let model = createModel cursorStart
    model.LastSearch <- search
    let expected = createModel cursorStart

    MainLogic.Cursor.searchNext reverse model

    expected.LastSearch <- search
    expected.Status <- search |> Option.map (MainStatus.search expectedResult.Count)
    assertEqualExceptCursor expected model
    SearchResult.fromModel model |> shouldEqual expectedResult

let assertSearch = assertSearchGiven false
let assertSearchPrevious = assertSearchGiven true

[<Test>]
let ``Search that matches nothing should not change the cursor``() =
    assertSearch "abc" 1 { Cursor = 1; Count = 0 }

[<Test>]
let ``Search that matches only the current node should not change the cursor``() =
    assertSearch "ob" 1 { Cursor = 1; Count = 1 }

[<Test>]
let ``Search that matches the current and next node should set the cursor to the next index``() =
    assertSearch "a" 2 { Cursor = 3; Count = 4 }

[<Test>]
let ``Search that matches a node wrapping around should set the cursor to the that index``() =
    assertSearch "ob" 2 { Cursor = 1; Count = 1 }

[<Test>]
let ``Search empy string matches nothing``() =
    assertSearch "" 2 { Cursor = 2; Count = 0 }


[<Test>]
let ``Search previous that matches nothing should not change the cursor``() =
    assertSearchPrevious "abc" 1 { Cursor = 1; Count = 0 }

[<Test>]
let ``Search previous that matches only the current node should not change the cursor``() =
    assertSearchPrevious "ob" 1 { Cursor = 1; Count = 1 }

[<Test>]
let ``Search previous that matches the current and previous node should set the cursor to the next index``() =
    assertSearchPrevious "a" 3 { Cursor = 2; Count = 4 }

[<Test>]
let ``Search previous that matches a node wrapping around should set the cursor to the that index``() =
    assertSearchPrevious "rys" 2 { Cursor = 3; Count = 1 }
