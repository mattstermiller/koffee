
namespace Koffee.Tests

open System
open System.Windows.Input
open FSharp.Desktop.UI
open NUnit.Framework
open FsUnitTyped
open Foq
open KellermanSoftware.CompareNetObjects
open Koffee
open Testing

[<TestFixture>]
type ``MainController tests for deleting files and folders``() =
    let oldNodes = [
        {Path = Path "path1"; Name = "one"; Type = Folder; Modified = None; Size = None}
        {Path = Path "path2"; Name = "two"; Type = Folder; Modified = None; Size = None}
    ]

    let newNodes = [oldNodes.[0]]

    let CreateModel () =
        let model = Model.Create<MainModel>()
        model.Path <- Path "path"
        model.Nodes <- oldNodes
        model.Cursor <- 1
        model.CommandText <- ""
        model.BackStack <- [Path "back", 8]
        model.ForwardStack <- [Path "fwd", 9]
        model

    let CreateFileSys () =
        Mock<IFileSystemService>()
            .Setup(fun x -> <@ x.GetNodes (Path "path") @>).Returns(newNodes)
            .Create()

    let CreateUnauthorizedFileSys () =
        Mock<IFileSystemService>()
            .Setup(fun x -> <@ x.GetNodes (Path "path") @>).Returns(newNodes)
            .Setup(fun x -> <@ x.Delete (any()) @>).Raises<UnauthorizedAccessException>()
            .Setup(fun x -> <@ x.DeletePermanently (any()) @>).Raises<UnauthorizedAccessException>()
            .Create()

    let CreateController fileSys =
        let settingsFactory () = Mock.Of<Mvc<SettingsEvents, SettingsModel>>()
        MainController(fileSys, settingsFactory)

    [<TestCase(0)>]
    [<TestCase(1)>]
    member x.``Delete calls file sys delete and sets message`` cursor =
        let fileSys = CreateFileSys()
        let contr = CreateController fileSys
        let model = CreateModel()
        model.Cursor <- cursor
        contr.Delete model

        let expectedNode = oldNodes.[cursor]
        verify <@ fileSys.Delete expectedNode @> once
        let expected = CreateModel()
        expected.CommandInputMode <- None
        expected.Nodes <- newNodes
        expected.Cursor <- 0
        expected.Status <- MainController.DeletedStatus false expectedNode.Type expectedNode.Name
        assertAreEqual expected model

    [<Test>]
    member x.``Delete handles error by setting error status``() =
        let fileSys = CreateUnauthorizedFileSys()
        let contr = CreateController fileSys
        let model = CreateModel()
        contr.Delete model

        let expected = CreateModel()
        expected.CommandInputMode <- None
        expected.IsErrorStatus <- true
        CompareLogic()
            |> ignoreMembers ["Status"]
            |> assertAreEqualWith expected model

    [<TestCase(0)>]
    [<TestCase(1)>]
    member x.``DeletePermanently prompt answered "y" calls file sys delete and sets message`` cursor =
        let fileSys = CreateFileSys()
        let contr = CreateController fileSys
        let model = CreateModel()
        model.Cursor <- cursor
        model.CommandInputMode <- Some DeletePermanently
        contr.CommandCharTyped 'y' model

        let expectedNode = oldNodes.[cursor]
        verify <@ fileSys.DeletePermanently expectedNode @> once
        let expected = CreateModel()
        expected.CommandInputMode <- None
        expected.Nodes <- newNodes
        expected.Cursor <- 0
        expected.Status <- MainController.DeletedStatus true expectedNode.Type expectedNode.Name
        assertAreEqual expected model

    [<Test>]
    member x.``DeletePermanently prompt answered "y" handles error by setting error status``() =
        let fileSys = CreateUnauthorizedFileSys()
        let contr = CreateController fileSys
        let model = CreateModel()
        model.CommandInputMode <- Some DeletePermanently
        contr.CommandCharTyped 'y' model

        let expected = CreateModel()
        expected.CommandInputMode <- None
        expected.IsErrorStatus <- true
        CompareLogic()
            |> ignoreMembers ["Status"]
            |> assertAreEqualWith expected model

    [<TestCase('n')>]
    [<TestCase('x')>]
    [<TestCase('q')>]
    member x.``DeletePermanently prompt answered with any key besides "y" escapes input mode`` char =
        let fileSys = CreateFileSys()
        let contr = CreateController fileSys
        let model = CreateModel()
        model.CommandInputMode <- Some DeletePermanently
        contr.CommandCharTyped char model

        verify <@ fileSys.DeletePermanently (any()) @> never
        let expected = CreateModel()
        expected.CommandInputMode <- None
        expected.Status <- MainController.DeleteCancelled
        assertAreEqual expected model
