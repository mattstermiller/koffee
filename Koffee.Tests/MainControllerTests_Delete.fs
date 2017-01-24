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
        .Setup(fun x -> <@ x.Recycle (any()) @>).Raises(ex)
        .Setup(fun x -> <@ x.Delete (any()) @>).Raises(ex)
        .Create()

let createController fileSys =
    let settingsFactory () = Mock.Of<Mvc<SettingsEvents, SettingsModel>>()
    MainController(fileSys, settingsFactory)

[<TestCase(0)>]
[<TestCase(1)>]
let ``Recycle calls file sys recycle and sets message`` cursor =
    let fileSys = createFileSys()
    let contr = createController fileSys
    let model = createModel()
    model.Cursor <- cursor
    contr.Recycle model |> Async.RunSynchronously

    let expectedNode = oldNodes.[cursor]
    verify <@ fileSys.Recycle expectedNode @> once
    let expectedAction = DeletedItem (oldNodes.[cursor], false)
    let expected = createModel()
    expected.CommandInputMode <- None
    expected.Nodes <- newNodes
    expected.Cursor <- 0
    expected.UndoStack <- expectedAction :: expected.UndoStack
    expected.RedoStack <- []
    expected.Status <- MainController.ActionStatus expectedAction
    assertAreEqual expected model

[<Test>]
let ``Recycle sets status message when not recyclable``() =
    let fileSys = createNoRecycleFileSys()
    let contr = createController fileSys
    let model = createModel()
    contr.Recycle model |> Async.RunSynchronously

    let expected = createModel()
    expected.SetErrorStatus (MainController.CannotRecycleStatus oldNodes.[1])
    assertAreEqual expected model

[<Test>]
let ``Recycle handles error by setting error status``() =
    let fileSys = createUnauthorizedFileSys()
    let contr = createController fileSys
    let model = createModel()
    contr.Recycle model |> Async.RunSynchronously

    let expected = createModel()
    expected |> MainController.SetActionExceptionStatus (DeletedItem (oldNodes.[1], false)) ex
    assertAreEqual expected model


[<TestCase(0)>]
[<TestCase(1)>]
let ``Delete prompt answered "y" calls file sys delete and sets message`` cursor =
    let fileSys = createNoRecycleFileSys()
    let contr = createController fileSys
    let model = createModel()
    model.Cursor <- cursor
    model.CommandInputMode <- Some Delete
    contr.CommandCharTyped 'y' model

    let expectedNode = oldNodes.[cursor]
    verify <@ fileSys.Delete expectedNode @> once
    let expectedAction = DeletedItem (oldNodes.[cursor], true)
    let expected = createModel()
    expected.CommandInputMode <- None
    expected.Nodes <- newNodes
    expected.Cursor <- 0
    expected.UndoStack <- expectedAction :: expected.UndoStack
    expected.RedoStack <- []
    expected.Status <- MainController.ActionStatus expectedAction
    assertAreEqual expected model

[<Test>]
let ``Delete prompt answered "y" handles error by setting error status``() =
    let fileSys = createUnauthorizedFileSys()
    let contr = createController fileSys
    let model = createModel()
    model.CommandInputMode <- Some Delete
    contr.CommandCharTyped 'y' model

    let expected = createModel()
    expected.CommandInputMode <- None
    expected |> MainController.SetActionExceptionStatus (DeletedItem (oldNodes.[1], true)) ex
    expected.IsErrorStatus <- true
    assertAreEqual expected model

[<TestCase('n')>]
[<TestCase('x')>]
[<TestCase('q')>]
let ``Delete prompt answered with any key besides "y" escapes input mode`` char =
    let fileSys = createFileSys()
    let contr = createController fileSys
    let model = createModel()
    model.CommandInputMode <- Some Delete
    contr.CommandCharTyped char model

    verify <@ fileSys.Delete (any()) @> never
    let expected = createModel()
    expected.CommandInputMode <- None
    expected.Status <- MainController.CancelledStatus
    assertAreEqual expected model
