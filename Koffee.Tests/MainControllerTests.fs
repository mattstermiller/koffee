namespace Koffee.Tests

open System.Windows.Input
open FSharp.Desktop.UI
open NUnit.Framework
open FsUnit
open Foq
open Koffee

[<TestFixture>]
type MainControllerFindTests() =
    let Nodes names =
        let toNode name = {
            Name = name
            Path = Path ""
            Type = Folder
            Modified = None
            Size = None
        }
        names |> Seq.map toNode |> Seq.toList

    let Find char cursorStart =
        let model = MainModel.Create<MainModel>()
        model.Nodes <- "alice,bob,charlie,crystal,apple,cherry".Split(',') |> Nodes
        model.Cursor <- cursorStart

        let pathing = Mock.Of<IPathService>()
        let settingsFactory () = Mock.Of<Mvc<SettingsEvents, SettingsModel>>()
        let contr = MainController(pathing, settingsFactory)
        contr.Find char model
        model.Cursor

    [<Test>]
    member x.``Find a char that matches the next node should set the cursor to the next index``() =
        Find 'c' 2 |> should equal 3

    [<Test>]
    member x.``Find a char that matches only the current node should not change the cursor``() =
        Find 'b' 1 |> should equal 1

    [<Test>]
    member x.``Find a char that matches a node wrapping around should set the cursor to the that index``() =
        Find 'b' 2 |> should equal 1


[<TestFixture>]
type MainControllerHistoryTests() =
    let Controller () =
        let pathing =
            Mock<IPathService>()
                .Setup(fun x -> <@ x.Normalize(any()) @>).Calls<Path>(fun p -> p)
                .Setup(fun x -> <@ x.GetNodes(any()) @>).Returns([])
                .Create()
        let settingsFactory () = Mock.Of<Mvc<SettingsEvents, SettingsModel>>()
        MainController(pathing, settingsFactory)

    let History contrFunc pathStr backStack forwardStack =
        let model = MainModel.Create<MainModel>()
        model.BackStack <- backStack |> List.map Path
        model.ForwardStack <- forwardStack |> List.map Path
        model.Path <- Path pathStr
        let contr = Controller()
        contrFunc contr model
        model

    let Back = History (fun contr -> contr.Back)
    let Forward = History (fun contr -> contr.Forward)

    [<Test>]
    member x.``Back without history does nothing``() =
        let model = Back "c:" [] []
        model.Path |> should equal (Path "c:")
        model.BackStack |> should equal []
        model.ForwardStack |> should equal []

    [<Test>]
    member x.``Back with simple history changes path and stacks``() =
        let model = Back "orig" ["back"] []
        model.Path |> should equal (Path "back")
        model.BackStack |> should equal []
        model.ForwardStack |> should equal [Path "orig"]

    [<Test>]
    member x.``Back with more history changes path and stacks``() =
        let model = Back "orig" ["back1"; "back2"] ["fwd1"; "fwd2"]
        model.Path |> should equal (Path "back1")
        model.BackStack |> should equal [Path "back2"]
        model.ForwardStack |> should equal (["orig"; "fwd1"; "fwd2"] |> List.map Path)

    [<Test>]
    member x.``Forward without history does nothing``() =
        let model = Forward "c:" [] []
        model.Path |> should equal (Path "c:")
        model.BackStack |> should equal []
        model.ForwardStack |> should equal []

    [<Test>]
    member x.``Forward with simple history changes path and stacks``() =
        let model = Forward "orig" [] ["fwd"]
        model.Path |> should equal (Path "fwd")
        model.BackStack |> should equal [Path "orig"]
        model.ForwardStack |> should equal []

    [<Test>]
    member x.``Forward with more history changes path and stacks``() =
        let model = Forward "orig" ["back1"; "back2"] ["fwd1"; "fwd2"]
        model.Path |> should equal (Path "fwd1")
        model.BackStack |> should equal (["orig"; "back1"; "back2"] |> List.map Path)
        model.ForwardStack |> should equal [Path "fwd2"]
