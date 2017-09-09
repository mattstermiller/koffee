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
    createNode "path" "file 1"
    createNode "path" "file 2"
    createNode "path" "file 3.txt"
]

let newNodes = [
    createNode "path" "file 1"
    createNode "path" "file 2 new"
    createNode "path" "file 3.txt"
]

let withParent path node =
    { node with Path = createPath (sprintf "%s/%s" path node.Name) }

let createModel () =
    let model = createBaseTestModel()
    model.Path <- createPath "path"
    model.Nodes <- oldNodes
    model.Cursor <- 0
    model.CommandTextSelection <- (1, 1)
    model

let ex = UnauthorizedAccessException()

let fileSysMock () =
    baseFileSysMock(newNodes)
        .Setup(fun x -> <@ x.GetNode (any()) @>).Returns(None)

let createController fileSys =
    let settingsFactory () = Mock.Of<Mvc<SettingsEvents, SettingsModel>>()
    MainController(fileSys, settingsFactory, Config(), None)


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
    model.Status <- Some <| ErrorMessage "prev error"
    if curPathDifferent then
        model.Path <- createPath "other"
        model.Cursor <- 5
    contr.Undo model |> Async.RunSynchronously

    let curPath = curNode.Path
    let prevPath = prevNode.Path
    verify <@ fileSys.Move curPath prevPath @> once
    let expected = createModel()
    expected.Nodes <- newNodes
    expected.Cursor <- 1
    expected.RedoStack <- action :: expected.RedoStack
    expected.Status <- Some <| MainStatus.undoAction action model.PathFormat
    if curPathDifferent then
        expected.BackStack <- (createPath "other", 5) :: expected.BackStack
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
    model.Path <- createPath "other"
    model.UndoStack <- action :: model.UndoStack
    contr.Undo model |> Async.RunSynchronously

    let expected = createModel()
    expected.Path <- createPath "other"
    expected |> MainStatus.setActionExceptionStatus (RenamedItem (curNode, prevNode.Name)) ex
    assertAreEqual expected model


[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo move item moves it back`` curPathDifferent =
    let fileSys = fileSysMock().Create()
    let contr = createController fileSys
    let prevNode = newNodes.[1]
    let curNode = newNodes.[1] |> withParent "current"
    let action = MovedItem (prevNode, curNode.Path)
    let otherItem = Some (createNode "other" "other", Move)
    let model = createModel()
    model.UndoStack <- action :: model.UndoStack
    model.ItemBuffer <- otherItem
    if curPathDifferent then
        model.Path <- createPath "other"
        model.Cursor <- 5
    contr.Undo model |> Async.RunSynchronously

    let curPath = curNode.Path
    let prevPath = prevNode.Path
    verify <@ fileSys.Move curPath prevPath @> once
    let expected = createModel()
    expected.Nodes <- newNodes
    expected.Cursor <- 1
    expected.RedoStack <- action :: expected.RedoStack
    expected.ItemBuffer <- otherItem
    expected.Status <- Some <| MainStatus.undoAction action model.PathFormat
    if curPathDifferent then
        expected.BackStack <- (createPath "other", 5) :: expected.BackStack
        expected.ForwardStack <- []
    assertAreEqual expected model


[<Test>]
let ``Undo move item handles error by setting error status and consumes action``() =
    let fileSys =
        fileSysMock()
            .Setup(fun x -> <@ x.Move (any()) (any()) @>).Raises(ex)
            .Create()
    let contr = createController fileSys
    let prevNode = newNodes.[1]
    let curNode = newNodes.[1] |> withParent "source"
    let action = MovedItem (prevNode, curNode.Path)
    let model = createModel()
    model.Path <- createPath "other"
    model.UndoStack <- action :: model.UndoStack
    contr.Undo model |> Async.RunSynchronously

    let expected = createModel()
    expected.Path <- createPath "other"
    expected |> MainStatus.setActionExceptionStatus (MovedItem (curNode, prevNode.Path)) ex
    assertAreEqual expected model


[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo copy item when copy has same timestamp deletes copy`` curPathDifferent =
    let modified = Some (DateTime(2000, 1, 1))
    let sourceNode = { (oldNodes.[1] |> withParent "source") with Modified = modified }
    let copyNode = oldNodes.[1]
    let refreshedCopyNode = { copyNode with Modified = modified }
    let fileSys =
        baseFileSysMock(newNodes)
            .Setup(fun x -> <@ x.GetNode copyNode.Path @>).Returns(Some refreshedCopyNode)
            .Create()
    let contr = createController fileSys
    let action = CopiedItem (sourceNode, copyNode.Path)
    let otherItem = Some (createNode "other" "other", Move)
    let model = createModel()
    model.UndoStack <- action :: model.UndoStack
    model.ItemBuffer <- otherItem
    model.Cursor <- 5
    if curPathDifferent then
        model.Path <- createPath "other"
    contr.Undo model |> Async.RunSynchronously

    let path = copyNode.Path
    verify <@ fileSys.Delete path @> once
    let expected = createModel()
    expected.RedoStack <- action :: expected.RedoStack
    expected.ItemBuffer <- otherItem
    expected.Status <- Some <| MainStatus.undoAction action model.PathFormat
    if curPathDifferent then
        expected.Path <- createPath "other"
        expected.Cursor <- 5
    else
        expected.Nodes <- newNodes
        expected.Cursor <- newNodes.Length - 1
    assertAreEqual expected model


[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo copy item when copy has different or no timestamp recycles copy`` hasTimestamp =
    let time = if hasTimestamp then Some (DateTime(2000, 1, 1)) else None
    let sourceNode = { (oldNodes.[1] |> withParent "source") with Modified = time }
    let copyNode = oldNodes.[1]
    let refreshedCopyNode = { copyNode with Modified = time |> Option.map (fun t -> t.AddDays(1.0)) }
    let fileSys =
        baseFileSysMock(newNodes)
            .Setup(fun x -> <@ x.GetNode copyNode.Path @>).Returns(Some refreshedCopyNode)
            .Create()
    let contr = createController fileSys
    let action = CopiedItem (sourceNode, copyNode.Path)
    let model = createModel()
    model.UndoStack <- action :: model.UndoStack
    model.Cursor <- 1
    contr.Undo model |> Async.RunSynchronously

    let path = copyNode.Path
    verify <@ fileSys.Recycle path @> once
    let expected = createModel()
    expected.Nodes <- newNodes
    expected.Cursor <- 1
    expected.RedoStack <- action :: expected.RedoStack
    expected.Status <- Some <| MainStatus.undoAction action model.PathFormat
    assertAreEqual expected model


[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo copy item handles error by setting error status and consumes action`` throwOnGetNode =
    let sourceNode = oldNodes.[1] |> withParent "source"
    let copyNode = oldNodes.[1]
    let fileSys =
        if throwOnGetNode then
            baseFileSysMock(newNodes)
                .Setup(fun x -> <@ x.GetNode (any()) @>).Raises(ex)
                .Create()
        else
            baseFileSysMock(newNodes)
                .Setup(fun x -> <@ x.GetNode (any()) @>).Returns(Some copyNode)
                .Setup(fun x -> <@ x.Recycle (any()) @>).Raises(ex)
                .Setup(fun x -> <@ x.Delete (any()) @>).Raises(ex)
                .Create()
    let contr = createController fileSys
    let action = CopiedItem (sourceNode, copyNode.Path)
    let model = createModel()
    model.UndoStack <- action :: model.UndoStack
    model.Path <- createPath "other"
    contr.Undo model |> Async.RunSynchronously

    let expected = createModel()
    expected.Path <- createPath "other"
    expected |> MainStatus.setActionExceptionStatus (DeletedItem (copyNode, false)) ex
    assertAreEqual expected model
