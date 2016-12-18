namespace Koffee.Tests

open System.Windows.Input
open FSharp.Desktop.UI
open NUnit.Framework
open FsUnitTyped
open Foq
open Koffee

[<TestFixture>]
type ``MainController tests for Find and Search``() =
    let Nodes names =
        let toNode name = {
            Name = name
            Path = Path ""
            Type = Folder
            Modified = None
            Size = None
        }
        names |> Seq.map toNode |> Seq.toList

    let CreateModel inputMode cursorStart =
        let model = MainModel.Create<MainModel>()
        model.Nodes <- "alice,bob,charlie,crystal,apple,cherry".Split(',') |> Nodes
        model.Cursor <- cursorStart
        model.CommandInputMode <- inputMode
        model

    let CreateController () =
        let fileSys = Mock.Of<IFileSystemService>()
        let settingsFactory () = Mock.Of<Mvc<SettingsEvents, SettingsModel>>()
        MainController(fileSys, settingsFactory)

    let Find char cursorStart =
        let model = CreateModel (Some Find) cursorStart

        let contr = CreateController()
        contr.CommandCharTyped char model

        model.CommandInputMode |> shouldEqual None
        model.LastFind |> shouldEqual (Some char)
        model.Status |> shouldEqual (MainController.FindStatus char)
        model.Cursor

    let Search searchStr cursorStart =
        let model = CreateModel (Some Search) cursorStart
        model.CommandText <- searchStr

        let contr = CreateController()
        contr.ExecuteCommand model

        model.CommandInputMode |> shouldEqual None
        model.LastSearch |> shouldEqual (Some searchStr)
        model.Status |> shouldEqual (MainController.SearchStatus searchStr)
        model.Cursor

    let SearchPrevious searchStr cursorStart =
        let model = CreateModel None cursorStart
        model.LastSearch <- Some searchStr

        let contr = CreateController()
        contr.SearchNext true model

        model.CommandInputMode |> shouldEqual None
        model.LastSearch |> shouldEqual (Some searchStr)
        model.Status |> shouldEqual (MainController.SearchStatus searchStr)
        model.Cursor

    [<Test>]
    member x.``Find a char that matches nothing should not change the cursor``() =
        Find 'A' 1 |> shouldEqual 1

    [<Test>]
    member x.``Find a char that matches only the current node should not change the cursor``() =
        Find 'b' 1 |> shouldEqual 1

    [<Test>]
    member x.``Find a char that matches the current and next node should set the cursor to the next index``() =
        Find 'c' 2 |> shouldEqual 3

    [<Test>]
    member x.``Find a char that matches a node wrapping around should set the cursor to the that index``() =
        Find 'b' 2 |> shouldEqual 1


    [<Test>]
    member x.``Search that matches nothing should not change the cursor``() =
        Search "abc" 1 |> shouldEqual 1

    [<Test>]
    member x.``Search that matches only the current node should not change the cursor``() =
        Search "ob" 1 |> shouldEqual 1

    [<Test>]
    member x.``Search that matches the current and next node should set the cursor to the next index``() =
        Search "a" 2 |> shouldEqual 3

    [<Test>]
    member x.``Search that matches a node wrapping around should set the cursor to the that index``() =
        Search "ob" 2 |> shouldEqual 1


    [<Test>]
    member x.``Search previous that matches nothing should not change the cursor``() =
        SearchPrevious "abc" 1 |> shouldEqual 1

    [<Test>]
    member x.``Search previous that matches only the current node should not change the cursor``() =
        SearchPrevious "ob" 1 |> shouldEqual 1

    [<Test>]
    member x.``Search previous that matches the current and previous node should set the cursor to the next index``() =
        SearchPrevious "a" 3 |> shouldEqual 2

    [<Test>]
    member x.``Search previous that matches a node wrapping around should set the cursor to the that index``() =
        SearchPrevious "rys" 2 |> shouldEqual 3
