module Koffee.MainControllerTests_FindSearch

open System.Windows.Input
open FSharp.Desktop.UI
open NUnit.Framework
open FsUnitTyped
open Foq
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
    }
    names |> Seq.map toNode |> Seq.toList

let createModel inputMode cursorStart =
    let model = createBaseTestModel()
    model.Nodes <- "alice,bob,charlie,crystal,apple,cherry".Split(',') |> nodes
    model.Cursor <- cursorStart
    model.CommandInputMode <- inputMode
    model

let createController () =
    let fileSys = Mock.Of<IFileSystemService>()
    let settingsFactory () = Mock.Of<Mvc<SettingsEvents, SettingsModel>>()
    MainController(fileSys, settingsFactory, Config(), None)

let assertEqualExceptCursor expected actual =
    CompareLogic()
        |> ignoreMembers ["Cursor"; "CommandText"]
        |> assertAreEqualWith expected actual

let find char cursorStart =
    let model = createModel (Some Find) cursorStart
    let expected = createModel (Some Find) cursorStart

    let contr = createController()
    Async.RunSynchronously <| contr.CommandCharTyped char model

    expected.CommandInputMode <- None
    expected.LastFind <- (Some char)
    expected.Status <- Some <| MainStatus.find char
    assertEqualExceptCursor expected model
    model.Cursor

let search searchStr cursorStart =
    let model = createModel (Some Search) cursorStart
    model.CommandText <- searchStr
    let expected = createModel (Some Search) cursorStart

    let contr = createController()
    contr.ExecuteCommand model

    expected.CommandInputMode <- None
    expected.LastSearch <- (Some searchStr)
    expected.Status <- Some <| MainStatus.search searchStr
    assertEqualExceptCursor expected model
    model.Cursor

let searchPrevious searchStr cursorStart =
    let model = createModel None cursorStart
    model.LastSearch <- Some searchStr
    let expected = createModel None cursorStart

    let contr = createController()
    contr.SearchNext true model

    expected.CommandInputMode <- None
    expected.LastSearch <- (Some searchStr)
    expected.Status <- Some <| MainStatus.search searchStr
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


[<Test>]
let ``Search that matches nothing should not change the cursor``() =
    search "abc" 1 |> shouldEqual 1

[<Test>]
let ``Search that matches only the current node should not change the cursor``() =
    search "ob" 1 |> shouldEqual 1

[<Test>]
let ``Search that matches the current and next node should set the cursor to the next index``() =
    search "a" 2 |> shouldEqual 3

[<Test>]
let ``Search that matches a node wrapping around should set the cursor to the that index``() =
    search "ob" 2 |> shouldEqual 1


[<Test>]
let ``Search previous that matches nothing should not change the cursor``() =
    searchPrevious "abc" 1 |> shouldEqual 1

[<Test>]
let ``Search previous that matches only the current node should not change the cursor``() =
    searchPrevious "ob" 1 |> shouldEqual 1

[<Test>]
let ``Search previous that matches the current and previous node should set the cursor to the next index``() =
    searchPrevious "a" 3 |> shouldEqual 2

[<Test>]
let ``Search previous that matches a node wrapping around should set the cursor to the that index``() =
    searchPrevious "rys" 2 |> shouldEqual 3
