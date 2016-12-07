namespace Koffee.Tests

open System
open System.Windows.Input
open FSharp.Desktop.UI
open NUnit.Framework
open FsUnit
open FsUnitTyped
open Foq
open Koffee

[<TestFixture>]
type ``MainController tests for opening paths``() =
    let CreateModel () =
        let model = Model.Create<MainModel>()
        model.Nodes <- [{Path = Path "old"; Name = "node"; Type = Folder; Modified = None; Size = None}]
        model.Path <- Path "old path"
        model.Cursor <- 2
        model.BackStack <- []
        model.ForwardStack <- [Path "fwd", 9]
        model

    let CreateController () =
        let nodes = [
            {Path = Path "path1"; Name = "one"; Type = Folder; Modified = None; Size = None}
            {Path = Path "path2"; Name = "two"; Type = Folder; Modified = None; Size = None}
        ]
        let fileSys =
            Mock<IFileSystemService>()
                .Setup(fun x -> <@ x.Normalize(Path "path") @>).Returns(Path "normalized")
                .Setup(fun x -> <@ x.GetNodes(Path "normalized") @>).Returns(nodes)
                .Setup(fun x -> <@ x.Normalize(Path "bad path") @>).Returns(Path "bad normalized")
                .Setup(fun x -> <@ x.GetNodes(Path "bad normalized") @>).Raises<UnauthorizedAccessException>()
                .Create()
        let settingsFactory () = Mock.Of<Mvc<SettingsEvents, SettingsModel>>()
        MainController(fileSys, settingsFactory)

    [<Test>]
    member x.``Opening a valid path updates model correctly``() =
        let model = CreateModel()
        let contr = CreateController()
        contr.OpenPath (Path "path") 1 model

        model.Nodes |> shouldHaveLength 2
        model.Path |> shouldEqual (Path "normalized")
        model.Cursor |> shouldEqual 1
        model.BackStack |> shouldEqual [Path "old path", 2]
        model.ForwardStack |> shouldEqual []
        model.Status |> shouldEqual ""
        model.IsErrorStatus |> shouldEqual false

    [<Test>]
    member x.``Opening a path that throws on GetNodes sets error status only``() =
        let model = CreateModel()
        let contr = CreateController()
        contr.OpenPath (Path "bad path") 0 model

        model.Nodes.Length |> shouldEqual 1
        model.Path |> shouldEqual (Path "old path")
        model.Cursor |> shouldEqual 2
        model.BackStack |> shouldEqual []
        model.ForwardStack |> shouldEqual [Path "fwd", 9]
        model.Status |> shouldContainText "Could not open path"
        model.Status |> shouldContainText "unauthorized"
        model.IsErrorStatus |> shouldEqual true
