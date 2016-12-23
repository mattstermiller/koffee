namespace Koffee.Tests

open System
open System.Windows.Input
open FSharp.Desktop.UI
open NUnit.Framework
open FsUnit
open FsUnitTyped
open Foq
open KellermanSoftware.CompareNetObjects
open Koffee
open Testing

[<TestFixture>]
type ``MainController tests for opening paths``() =
    let Node path name =
        {Path = Path path; Name = name; Type = Folder; Modified = None; Size = None}

    let newNodes = [
        Node "path1" "one"
        Node "path2" "two"
    ]

    let CreateModel () =
        let model = Model.Create<MainModel>()
        model.Nodes <- [
            Node "old/p1" "old one"
            Node "old/p2" "old two"
            Node "old/p3" "old three"
        ]
        model.Path <- Path "old"
        model.Cursor <- 2
        model.BackStack <- []
        model.ForwardStack <- [Path "fwd", 9]
        model.Status <- ""
        model

    let CreateController () =
        let fileSys =
            Mock<IFileSystemService>()
                .Setup(fun x -> <@ x.Normalize(Path "path") @>).Returns(Path "normalized")
                .Setup(fun x -> <@ x.GetNodes(Path "normalized") @>).Returns(newNodes)
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

        let expected = CreateModel()
        expected.Nodes <- newNodes
        expected.Path <- Path "normalized"
        expected.Cursor <- 1
        expected.BackStack <- [Path "old", 2]
        expected.ForwardStack <- []
        assertAreEqual expected model

    [<Test>]
    member x.``Opening a path that throws on GetNodes sets error status only``() =
        let model = CreateModel()
        let contr = CreateController()
        contr.OpenPath (Path "bad path") 0 model

        let expected = CreateModel()
        expected.IsErrorStatus <- true
        CompareLogic()
            |> ignoreMembers ["Status"]
            |> assertAreEqualWith expected model
        model.Status |> shouldContainText "Could not open path"
        model.Status |> shouldContainText "unauthorized"
