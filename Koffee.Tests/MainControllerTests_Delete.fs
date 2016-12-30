module Koffee.MainControllerTests_Delete

open System
open System.Windows.Input
open FSharp.Desktop.UI
open NUnit.Framework
open FsUnitTyped
open Foq
open KellermanSoftware.CompareNetObjects
open Testing

let node path name =
    {Path = Path path; Name = name; Type = Folder; Modified = None; Size = None}

let oldNodes = [
    node "path1" "one"
    node "path2" "two"
]

let newNodes = [oldNodes.[0]]

let createModel () =
    let model = Model.Create<MainModel>()
    model.Path <- Path "path"
    model.Nodes <- oldNodes
    model.Cursor <- 1
    model.CommandText <- ""
    model.BackStack <- [Path "back", 8]
    model.ForwardStack <- [Path "fwd", 9]
    model

let createFileSys () =
    Mock<IFileSystemService>()
        .Setup(fun x -> <@ x.GetNodes (Path "path") @>).Returns(newNodes)
        .Create()

let createUnauthorizedFileSys () =
    Mock<IFileSystemService>()
        .Setup(fun x -> <@ x.GetNodes (Path "path") @>).Returns(newNodes)
        .Setup(fun x -> <@ x.Delete (any()) @>).Raises<UnauthorizedAccessException>()
        .Setup(fun x -> <@ x.DeletePermanently (any()) @>).Raises<UnauthorizedAccessException>()
        .Create()

let createController fileSys =
    let settingsFactory () = Mock.Of<Mvc<SettingsEvents, SettingsModel>>()
    MainController(fileSys, settingsFactory)

[<TestCase(0)>]
[<TestCase(1)>]
let ``Delete calls file sys delete and sets message`` cursor =
    let fileSys = createFileSys()
    let contr = createController fileSys
    let model = createModel()
    model.Cursor <- cursor
    contr.Delete model

    let expectedNode = oldNodes.[cursor]
    verify <@ fileSys.Delete expectedNode @> once
    let expected = createModel()
    expected.CommandInputMode <- None
    expected.Nodes <- newNodes
    expected.Cursor <- 0
    expected.Status <- MainController.ActionStatus (DeletedItem (expectedNode, false))
    assertAreEqual expected model

[<Test>]
let ``Delete handles error by setting error status``() =
    let fileSys = createUnauthorizedFileSys()
    let contr = createController fileSys
    let model = createModel()
    contr.Delete model

    let expected = createModel()
    expected.CommandInputMode <- None
    expected.IsErrorStatus <- true
    CompareLogic()
        |> ignoreMembers ["Status"]
        |> assertAreEqualWith expected model

[<TestCase(0)>]
[<TestCase(1)>]
let ``DeletePermanently prompt answered "y" calls file sys delete and sets message`` cursor =
    let fileSys = createFileSys()
    let contr = createController fileSys
    let model = createModel()
    model.Cursor <- cursor
    model.CommandInputMode <- Some DeletePermanently
    contr.CommandCharTyped 'y' model

    let expectedNode = oldNodes.[cursor]
    verify <@ fileSys.DeletePermanently expectedNode @> once
    let expected = createModel()
    expected.CommandInputMode <- None
    expected.Nodes <- newNodes
    expected.Cursor <- 0
    expected.Status <- MainController.ActionStatus (DeletedItem (expectedNode, true))
    assertAreEqual expected model

[<Test>]
let ``DeletePermanently prompt answered "y" handles error by setting error status``() =
    let fileSys = createUnauthorizedFileSys()
    let contr = createController fileSys
    let model = createModel()
    model.CommandInputMode <- Some DeletePermanently
    contr.CommandCharTyped 'y' model

    let expected = createModel()
    expected.CommandInputMode <- None
    expected.IsErrorStatus <- true
    CompareLogic()
        |> ignoreMembers ["Status"]
        |> assertAreEqualWith expected model

[<TestCase('n')>]
[<TestCase('x')>]
[<TestCase('q')>]
let ``DeletePermanently prompt answered with any key besides "y" escapes input mode`` char =
    let fileSys = createFileSys()
    let contr = createController fileSys
    let model = createModel()
    model.CommandInputMode <- Some DeletePermanently
    contr.CommandCharTyped char model

    verify <@ fileSys.DeletePermanently (any()) @> never
    let expected = createModel()
    expected.CommandInputMode <- None
    expected.Status <- MainController.DeleteCancelledStatus
    assertAreEqual expected model
