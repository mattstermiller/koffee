namespace Koffee.Tests

open System.Windows.Input
open FSharp.Desktop.UI
open NUnit.Framework
open FsUnit
open Foq
open Koffee

[<TestFixture>]
type ``MainController tests for Back and Forward``() =
    let CreateController () =
        let fileSys =
            Mock<IFileSystemService>()
                .Setup(fun x -> <@ x.Normalize(any()) @>).Calls<Path>(fun p -> p)
                .Setup(fun x -> <@ x.GetNodes(any()) @>).Returns([])
                .Create()
        let settingsFactory () = Mock.Of<Mvc<SettingsEvents, SettingsModel>>()
        MainController(fileSys, settingsFactory)

    let History contrFunc pathStr cursor backStack forwardStack =
        let model = MainModel.Create<MainModel>()
        model.BackStack <- backStack |> List.map (fun (p, c) -> (Path p, c))
        model.ForwardStack <- forwardStack |> List.map (fun (p, c) -> (Path p, c))
        model.Path <- Path pathStr
        model.Cursor <- cursor
        model.Status <- "previous status"
        let contr = CreateController()
        contrFunc contr model
        model

    let Back = History (fun contr -> contr.Back)
    let Forward = History (fun contr -> contr.Forward)

    [<Test>]
    member x.``Back without history does nothing``() =
        let model = Back "c:" 1 [] []
        model.Path |> should equal (Path "c:")
        model.Cursor |> should equal 1
        model.BackStack |> should equal []
        model.ForwardStack |> should equal []
        model.Status |> should equal "previous status"

    [<Test>]
    member x.``Back with simple history changes path and stacks``() =
        let model = Back "orig" 1 ["back", 2] []
        model.Path |> should equal (Path "back")
        model.Cursor |> should equal 2
        model.BackStack |> should equal []
        model.ForwardStack |> should equal [Path "orig", 1]
        model.Status |> should equal ""

    [<Test>]
    member x.``Back with more history changes path and stacks``() =
        let model = Back "orig" 1 ["back1", 2; "back2", 3] ["fwd1", 4; "fwd2", 5]
        model.Path |> should equal (Path "back1")
        model.Cursor |> should equal 2
        model.BackStack |> should equal [Path "back2", 3]
        model.ForwardStack |> should equal [Path "orig", 1; Path "fwd1", 4; Path "fwd2", 5]
        model.Status |> should equal ""

    [<Test>]
    member x.``Forward without history does nothing``() =
        let model = Forward "c:" 1 [] []
        model.Path |> should equal (Path "c:")
        model.Cursor |> should equal 1
        model.BackStack |> should equal []
        model.ForwardStack |> should equal []
        model.Status |> should equal "previous status"

    [<Test>]
    member x.``Forward with simple history changes path and stacks``() =
        let model = Forward "orig" 1 [] ["fwd", 2]
        model.Path |> should equal (Path "fwd")
        model.Cursor |> should equal 2
        model.BackStack |> should equal [Path "orig", 1]
        model.ForwardStack |> should equal []
        model.Status |> should equal ""

    [<Test>]
    member x.``Forward with more history changes path and stacks``() =
        let model = Forward "orig" 1 ["back1", 2; "back2", 3] ["fwd1", 4; "fwd2", 5]
        model.Path |> should equal (Path "fwd1")
        model.Cursor |> should equal 4
        model.BackStack |> should equal [Path "orig", 1; Path "back1", 2; Path "back2", 3]
        model.ForwardStack |> should equal [Path "fwd2", 5]
        model.Status |> should equal ""
