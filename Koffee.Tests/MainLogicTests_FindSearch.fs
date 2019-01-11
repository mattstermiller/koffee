module Koffee.MainLogicTests_FindSearch

open NUnit.Framework
open FsUnitTyped
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

    MainLogic.Cursor.find_m true char model

    expected.LastFind <- Some (true, char)
    expected.Status <- Some <| MainStatus.find true char
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
    static member fromModel (model: MainBindModel) =
        { Cursor = model.Cursor
          Count = model.Nodes |> Seq.filter (fun n -> n.IsSearchMatch) |> Seq.length }

let assertSearchGiven reverse searchStr cursorStart expectedResult =
    let search = if searchStr <> "" then Some (false, searchStr) else None
    let model = createModel cursorStart
    model.LastSearch <- search
    let expected = createModel cursorStart

    MainLogic.Cursor.searchNext_m reverse model

    expected.LastSearch <- search
    expected.Status <- search |> Option.map (fun (cs, s) -> MainStatus.search expectedResult.Count cs s)
    SearchResult.fromModel model |> shouldEqual expectedResult
    assertEqualExceptCursor expected model

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


let firstPart (s: string) = s.Split([|'/'|]).[0]

[<TestCase("")>]
[<TestCase("needle")>]
[<TestCase("hay stack/")>]
let ``parseSearch parses string without switch`` input =
    MainLogic.Cursor.parseSearch input |> shouldEqual (Ok (firstPart input, None))

[<TestCase("/i")>]
[<TestCase("hay stack/ic")>]
let ``parseSearch parses string with case-insensitive switch`` input =
    MainLogic.Cursor.parseSearch input |> shouldEqual (Ok (firstPart input, Some false))

[<TestCase("/c")>]
[<TestCase("hay stack/ci")>]
let ``parseSearch parses string with case-sensitive switch`` input =
    MainLogic.Cursor.parseSearch input |> shouldEqual (Ok (firstPart input, Some true))

[<TestCase("needle/a")>]
[<TestCase("needle/ia")>]
let ``parseSearch returns error for invalid switch`` input =
    MainLogic.Cursor.parseSearch input |> shouldEqual (Error <| InvalidSearchSwitch 'a')

[<TestCase("some/thing/")>]
[<TestCase("some/thing/i")>]
[<TestCase("some/other/thing/i")>]
let ``parseSearch returns error for more than one switch delimiter`` input =
    MainLogic.Cursor.parseSearch input |> shouldEqual (Error InvalidSearchSlash)
