module Koffee.MainControllerTests_UndoRedo

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
    createNode "path" "three.txt"
]

let newNodes = [
    createNode "path" "one"
    createNode "path" "four"
    createNode "path" "three.txt"
]

let createModel () =
    let model = createBaseTestModel()
    model.Path <- Path "path"
    model.Nodes <- oldNodes
    model.Cursor <- 0
    model.CommandTextSelection <- (1, 1)
    model

let ex = UnauthorizedAccessException()

let fileSysMock () =
    baseFileSysMock newNodes

let createController fileSys =
    let settingsFactory () = Mock.Of<Mvc<SettingsEvents, SettingsModel>>()
    MainController(fileSys, settingsFactory)


[<Test>]
let ``Undo with empty undo stack sets status only``() =
    let fileSys = Mock.Of<IFileSystemService>()
    let contr = createController fileSys
    let model = createModel()
    model.UndoStack <- []
    contr.Undo model

    let expected = createModel()
    expected.UndoStack <- []
    expected.Status <- MainController.NoUndoActionsStatus
    assertAreEqual expected model

[<Test>]
let ``Redo with empty stack sets status only``() =
    let fileSys = Mock.Of<IFileSystemService>()
    let contr = createController fileSys
    let model = createModel()
    model.RedoStack <- []
    contr.Redo model

    let expected = createModel()
    expected.RedoStack <- []
    expected.Status <- MainController.NoRedoActionsStatus
    assertAreEqual expected model


[<TestCase(1, false)>]
[<TestCase(2, false)>]
[<TestCase(1, true)>]
let ``Undo create item deletes if empty`` nodeIndex curPathDifferent =
    let fileSys =
        fileSysMock()
            .Setup(fun x -> <@ x.IsEmpty (any()) @>).Returns(true)
            .Create()
    let contr = createController fileSys
    let createdNode = oldNodes.[nodeIndex]
    let action = CreatedItem createdNode
    let model = createModel()
    model.UndoStack <- action :: model.UndoStack
    model.IsErrorStatus <- true
    model.Cursor <- 5
    if curPathDifferent then
        model.Path <- Path "other"
    contr.Undo model

    let expectedPath = createdNode.Path
    verify <@ fileSys.Delete expectedPath @> once
    let expected = createModel()
    expected.RedoStack <- action :: expected.RedoStack
    expected.Status <- MainController.UndoActionStatus action
    if curPathDifferent then
        expected.Path <- Path "other"
        expected.Cursor <- 5
    else
        expected.Nodes <- newNodes
        expected.Cursor <- newNodes.Length - 1
    assertAreEqual expected model

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo create item handles error by setting error status and consumes action`` isEmptyThrows =
    let fsMock =
        if isEmptyThrows then
            fileSysMock().Setup(fun x -> <@ x.IsEmpty (any()) @>).Raises(ex)
        else
            fileSysMock().Setup(fun x -> <@ x.IsEmpty (any()) @>).Returns(true)
    let fileSys =
        fsMock
            .Setup(fun x -> <@ x.Delete (any()) @>).Raises(ex)
            .Create()
    let contr = createController fileSys
    let model = createModel()
    let action = CreatedItem oldNodes.[0]
    model.Path <- Path "other"
    model.UndoStack <- action :: model.UndoStack
    contr.Undo model

    let expected = createModel()
    expected.Path <- Path "other"
    expected |> MainController.SetActionExceptionStatus (DeletedItem (oldNodes.[0], true)) ex
    assertAreEqual expected model

[<Test>]
let ``Undo create item sets status if non-empty and consumes action``() =
    let fileSys =
        fileSysMock()
            .Setup(fun x -> <@ x.IsEmpty (any()) @>).Returns(false)
            .Create()
    let contr = createController fileSys
    let createdNode = oldNodes.[0]
    let action = CreatedItem createdNode
    let model = createModel()
    model.Path <- Path "other"
    model.UndoStack <- action :: model.UndoStack
    contr.Undo model

    verify <@ fileSys.Delete (any()) @> never
    let expected = createModel()
    expected.Path <- Path "other"
    expected.SetErrorStatus (MainController.CannotUndoNonEmptyCreatedStatus createdNode)
    assertAreEqual expected model

[<TestCase(false)>]
[<TestCase(true)>]
let ``Redo create item creates item`` curPathDifferent =
    let fileSys = fileSysMock().Create()
    let contr = createController fileSys
    let createdNode = newNodes.[1]
    let action = CreatedItem createdNode
    let model = createModel()
    model.RedoStack <- action :: model.RedoStack
    if curPathDifferent then
        model.Path <- Path "other"
        model.Cursor <- 5
    contr.Redo model

    let nodeType = createdNode.Type
    let path = createdNode.Path
    verify <@ fileSys.Create nodeType path @> once
    let expected = createModel()
    expected.Nodes <- newNodes
    expected.Cursor <- 1
    expected.Status <- MainController.RedoActionStatus action
    expected.UndoStack <- action :: expected.UndoStack
    if curPathDifferent then
        expected.BackStack <- (Path "other", 5) :: expected.BackStack
        expected.ForwardStack <- []
    assertAreEqual expected model


[<TestCase(true)>]
[<TestCase(false)>]
let ``Undo rename item names file back to original`` curPathDifferent =
    let fileSys = fileSysMock().Create()
    let contr = createController fileSys
    let prevNode = newNodes.[1]
    let curNode = oldNodes.[1]
    let action = RenamedItem (prevNode, curNode.Name)
    let model = createModel()
    model.UndoStack <- action :: model.UndoStack
    model.IsErrorStatus <- true
    if curPathDifferent then
        model.Path <- Path "other"
        model.Cursor <- 5
    contr.Undo model

    let curPath = curNode.Path
    let prevPath = prevNode.Path
    verify <@ fileSys.Move curPath prevPath @> once
    let expected = createModel()
    expected.Nodes <- newNodes
    expected.Cursor <- 1
    expected.RedoStack <- action :: expected.RedoStack
    expected.Status <- MainController.UndoActionStatus action
    if curPathDifferent then
        expected.BackStack <- (Path "other", 5) :: expected.BackStack
        expected.ForwardStack <- []
    assertAreEqual expected model

[<Test>]
let ``Undo rename item handles error by setting error status and consumes action``() =
    let fileSys =
        fileSysMock()
            .Setup(fun x -> <@ x.Move (any()) (any()) @>).Raises(ex)
            .Create()
    let contr = createController fileSys
    let prevNode = newNodes.[1]
    let curNode = oldNodes.[1]
    let action = RenamedItem (prevNode, curNode.Name)
    let model = createModel()
    model.Path <- Path "other"
    model.UndoStack <- action :: model.UndoStack
    contr.Undo model

    let expected = createModel()
    expected.Path <- Path "other"
    expected |> MainController.SetActionExceptionStatus (RenamedItem (curNode, prevNode.Name)) ex
    assertAreEqual expected model

[<TestCase(false)>]
[<TestCase(true)>]
let ``Redo rename item renames original file name again`` curPathDifferent =
    let fileSys = fileSysMock().Create()
    let contr = createController fileSys
    let newNode = newNodes.[1]
    let curNode = oldNodes.[1]
    let action = RenamedItem (curNode, newNode.Name)
    let model = createModel()
    model.RedoStack <- action :: model.RedoStack
    if curPathDifferent then
        model.Path <- Path "other"
        model.Cursor <- 5
    contr.Redo model

    let curPath = curNode.Path
    let newPath = newNode.Path
    verify <@ fileSys.Move curPath newPath @> once
    let expected = createModel()
    expected.Nodes <- newNodes
    expected.Cursor <- 1
    expected.Status <- MainController.RedoActionStatus action
    expected.UndoStack <- action :: expected.UndoStack
    if curPathDifferent then
        expected.BackStack <- (Path "other", 5) :: expected.BackStack
        expected.ForwardStack <- []
    assertAreEqual expected model


[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo recycle or delete sets status message and consumes action`` permanent =
    let fileSys =
        fileSysMock()
            .Setup(fun x -> <@ x.IsEmpty (any()) @>).Returns(false)
            .Create()
    let contr = createController fileSys
    let deletedNode = createNode "path" "deleteMe"
    let undoAction = DeletedItem (deletedNode, permanent)
    let model = createModel()
    model.Path <- Path "other"
    model.UndoStack <- undoAction :: model.UndoStack
    contr.Undo model

    let expected = createModel()
    expected.Path <- Path "other"
    expected.SetErrorStatus (MainController.CannotUndoDeleteStatus permanent deletedNode)
    assertAreEqual expected model
