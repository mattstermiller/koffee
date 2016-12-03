namespace Koffee.Tests

open System.Windows.Input
open FSharp.Desktop.UI
open NUnit.Framework
open FsUnit
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
        model.CommandInputMode <- Some inputMode
        model

    let CreateController =
        let fileSys = Mock.Of<IFileSystemService>()
        let settingsFactory () = Mock.Of<Mvc<SettingsEvents, SettingsModel>>()
        MainController(fileSys, settingsFactory)

    let Find char cursorStart =
        let model = CreateModel Find cursorStart

        let contr = CreateController
        contr.CommandCharTyped char model

        model.CommandInputMode |> should equal None
        model.LastFind |> should equal (Some char)
        model.Status |> should equal ("Find " + (char.ToString()))
        model.Cursor

    let Search searchStr cursorStart =
        let model = CreateModel Search cursorStart
        model.CommandText <- searchStr

        let contr = CreateController
        contr.ExecuteCommand model

        model.CommandInputMode |> should equal None
        model.LastSearch |> should equal (Some searchStr)
        model.Status |> should equal (sprintf "Search \"%s\"" searchStr)
        model.Cursor

    [<Test>]
    member x.``Find a char that matches nothing should not change the cursor``() =
        Find 'A' 1 |> should equal 1

    [<Test>]
    member x.``Find a char that matches only the current node should not change the cursor``() =
        Find 'b' 1 |> should equal 1

    [<Test>]
    member x.``Find a char that matches the next node should set the cursor to the next index``() =
        Find 'c' 2 |> should equal 3

    [<Test>]
    member x.``Find a char that matches a node wrapping around should set the cursor to the that index``() =
        Find 'b' 2 |> should equal 1


    [<Test>]
    member x.``Search that matches nothing should not change the cursor``() =
        Search "abc" 1 |> should equal 1

    [<Test>]
    member x.``Search that matches only the current node should not change the cursor``() =
        Search "ob" 1 |> should equal 1

    [<Test>]
    member x.``Search that matches the next node should set the cursor to the next index``() =
        Search "ry" 2 |> should equal 3

    [<Test>]
    member x.``Search that matches a node wrapping around should set the cursor to the that index``() =
        Search "ob" 2 |> should equal 1
