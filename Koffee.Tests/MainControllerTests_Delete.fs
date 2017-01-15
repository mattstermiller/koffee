module Koffee.MainControllerTests_Delete

open System
open System.Windows.Input
open FSharp.Desktop.UI
open NUnit.Framework
open FsUnitTyped
open Foq
open KellermanSoftware.CompareNetObjects
open Testing

let oldNodes = [
    createNode "path" "one"
    createNode "path" "two"
]

let newNodes = [oldNodes.[0]]

let createModel () =
    let model = createBaseTestModel()
    model.Path <- Path "path"
    model.Nodes <- oldNodes
    model.Cursor <- 1
    model

let ex = UnauthorizedAccessException()

let createFileSys () =
    (baseFileSysMock newNodes)
        .Setup(fun x -> <@ x.IsRecyclable (any()) @>).Returns(true)
        .Create()

let createNoRecycleFileSys () =
    (baseFileSysMock newNodes)
        .Setup(fun x -> <@ x.IsRecyclable (any()) @>).Returns(false)
        .Create()

let createUnauthorizedFileSys () =
    (baseFileSysMock newNodes)
        .Setup(fun x -> <@ x.IsRecyclable (any()) @>).Returns(true)
        .Setup(fun x -> <@ x.Delete (any()) @>).Raises(ex)
        .Setup(fun x -> <@ x.DeletePermanently (any()) @>).Raises(ex)
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
    contr.Delete model |> Async.RunSynchronously

    let expectedNode = oldNodes.[cursor]
    verify <@ fileSys.Delete expectedNode @> once
    let expected = createModel()
    expected.CommandInputMode <- None
    expected.Nodes <- newNodes
    expected.Cursor <- 0
    expected.UndoStack <- DeletedItem (oldNodes.[cursor], false) :: expected.UndoStack
    expected.RedoStack <- []
    expected.Status <- MainController.ActionStatus (DeletedItem (expectedNode, false))
    assertAreEqual expected model

[<Test>]
let ``Delete sets status message when not recyclable``() =
    let fileSys = createNoRecycleFileSys()
    let contr = createController fileSys
    let model = createModel()
    contr.Delete model |> Async.RunSynchronously

    let expected = createModel()
    expected.SetErrorStatus (MainController.CannotDeleteUnrecyclableStatus oldNodes.[1])
    assertAreEqual expected model

[<Test>]
let ``Delete handles error by setting error status``() =
    let fileSys = createUnauthorizedFileSys()
    let contr = createController fileSys
    let model = createModel()
    contr.Delete model |> Async.RunSynchronously

    let expected = createModel()
    expected |> MainController.SetActionExceptionStatus (DeletedItem (oldNodes.[1], false)) ex
    assertAreEqual expected model

[<TestCase(0)>]
[<TestCase(1)>]
let ``DeletePermanently prompt answered "y" calls file sys delete and sets message`` cursor =
    let fileSys = createNoRecycleFileSys()
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
    expected.UndoStack <- DeletedItem (oldNodes.[cursor], true) :: expected.UndoStack
    expected.RedoStack <- []
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
    expected |> MainController.SetActionExceptionStatus (DeletedItem (oldNodes.[1], true)) ex
    expected.IsErrorStatus <- true
    assertAreEqual expected model

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
