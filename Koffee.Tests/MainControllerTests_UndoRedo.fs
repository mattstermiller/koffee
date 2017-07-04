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
    baseFileSysMock newNodes

let createController fileSys =
    let settingsFactory () = Mock.Of<Mvc<SettingsEvents, SettingsModel>>()
    MainController(fileSys, settingsFactory, Config())


[<Test>]
let ``Undo with empty undo stack sets status only``() =
    let fileSys = Mock.Of<IFileSystemService>()
    let contr = createController fileSys
    let model = createModel()
    model.UndoStack <- []
    contr.Undo model |> Async.RunSynchronously

    let expected = createModel()
    expected.UndoStack <- []
    expected.Status <- Some <| MainStatus.noUndoActions
    assertAreEqual expected model

[<Test>]
let ``Redo with empty stack sets status only``() =
    let fileSys = Mock.Of<IFileSystemService>()
    let contr = createController fileSys
    let model = createModel()
    model.RedoStack <- []
    contr.Redo model |> Async.RunSynchronously

    let expected = createModel()
    expected.RedoStack <- []
    expected.Status <- Some <| MainStatus.noRedoActions
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
    model.Status <- Some <| ErrorMessage "prev error"
    model.Cursor <- 5
    if curPathDifferent then
        model.Path <- createPath "other"
    contr.Undo model |> Async.RunSynchronously

    let expectedPath = createdNode.Path
    verify <@ fileSys.Delete expectedPath @> once
    let expected = createModel()
    expected.RedoStack <- action :: expected.RedoStack
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
    model.Path <- createPath "other"
    model.UndoStack <- action :: model.UndoStack
    contr.Undo model |> Async.RunSynchronously

    let expected = createModel()
    expected.Path <- createPath "other"
    expected |> MainStatus.setActionExceptionStatus (DeletedItem (oldNodes.[0], true)) ex
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
    model.Path <- createPath "other"
    model.UndoStack <- action :: model.UndoStack
    contr.Undo model |> Async.RunSynchronously

    verify <@ fileSys.Delete (any()) @> never
    let expected = createModel()
    expected.Path <- createPath "other"
    expected.Status <- Some <| MainStatus.cannotUndoNonEmptyCreated createdNode
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
        model.Path <- createPath "other"
        model.Cursor <- 5
    contr.Redo model |> Async.RunSynchronously

    let nodeType = createdNode.Type
    let path = createdNode.Path
    verify <@ fileSys.Create nodeType path @> once
    let expected = createModel()
    expected.Nodes <- newNodes
    expected.Cursor <- 1
    expected.Status <- Some <| MainStatus.redoAction action model.PathFormat
    expected.UndoStack <- action :: expected.UndoStack
    if curPathDifferent then
        expected.BackStack <- (createPath "other", 5) :: expected.BackStack
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
let ``Redo rename item renames original file name again`` curPathDifferent =
    let fileSys = fileSysMock().Create()
    let contr = createController fileSys
    let newNode = newNodes.[1]
    let curNode = oldNodes.[1]
    let action = RenamedItem (curNode, newNode.Name)
    let model = createModel()
    model.RedoStack <- action :: model.RedoStack
    if curPathDifferent then
        model.Path <- createPath "other"
        model.Cursor <- 5
    contr.Redo model |> Async.RunSynchronously

    let curPath = curNode.Path
    let newPath = newNode.Path
    verify <@ fileSys.Move curPath newPath @> once
    let expected = createModel()
    expected.Nodes <- newNodes
    expected.Cursor <- 1
    expected.Status <- Some <| MainStatus.redoAction action model.PathFormat
    expected.UndoStack <- action :: expected.UndoStack
    if curPathDifferent then
        expected.BackStack <- (createPath "other", 5) :: expected.BackStack
        expected.ForwardStack <- []
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
let ``Redo move item moves it from original path again`` curPathDifferent =
    let fileSys = fileSysMock().Create()
    let contr = createController fileSys
    let sourceNode = newNodes.[1] |> withParent "source"
    let newNode = newNodes.[1]
    let action = MovedItem (sourceNode, newNode.Path)
    let otherItem = Some (createNode "other" "other", Move)
    let model = createModel()
    model.RedoStack <- action :: model.RedoStack
    model.ItemBuffer <- otherItem
    if curPathDifferent then
        model.Path <- createPath "other"
        model.Cursor <- 5
    contr.Redo model |> Async.RunSynchronously

    let sourcePath = sourceNode.Path
    let name = newNode.Path
    verify <@ fileSys.Move sourcePath name @> once
    let expected = createModel()
    expected.Nodes <- newNodes
    expected.Cursor <- 1
    expected.UndoStack <- action :: expected.UndoStack
    expected.ItemBuffer <- otherItem
    expected.Status <- Some <| MainStatus.redoAction action model.PathFormat
    if curPathDifferent then
        expected.BackStack <- (createPath "other", 5) :: expected.BackStack
        expected.ForwardStack <- []
    assertAreEqual expected model



[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo copy item when copy has same timestamp deletes copy`` curPathDifferent =
    let modified = Some (DateTime(2000, 1, 1))
    let sourceNode = { (oldNodes.[1] |> withParent "source") with Modified = modified }
    let copyNode = oldNodes.[1]
    let refreshedCopyNode = { copyNode with Modified = modified }
    let fileSys =
        fileSysMock()
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
        fileSysMock()
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
            fileSysMock()
                .Setup(fun x -> <@ x.GetNode (any()) @>).Raises(ex)
                .Create()
        else
            fileSysMock()
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


[<TestCase(false)>]
[<TestCase(true)>]
let ``Redo copy item makes another copy`` curPathDifferent =
    let fileSys = fileSysMock().Create()
    let contr = createController fileSys
    let sourceNode = newNodes.[1] |> withParent "source"
    let copyNode = newNodes.[1]
    let action = CopiedItem (sourceNode, copyNode.Path)
    let otherItem = Some (createNode "other" "other", Move)
    let model = createModel()
    model.RedoStack <- action :: model.RedoStack
    model.ItemBuffer <- otherItem
    if curPathDifferent then
        model.Path <- createPath "other"
        model.Cursor <- 5
    contr.Redo model |> Async.RunSynchronously

    let sourcePath = sourceNode.Path
    let destPath = copyNode.Path
    verify <@ fileSys.Copy sourcePath destPath @> once
    let expected = createModel()
    expected.Nodes <- newNodes
    expected.Cursor <- 1
    expected.UndoStack <- action :: expected.UndoStack
    expected.ItemBuffer <- otherItem
    expected.Status <- Some <| MainStatus.redoAction action model.PathFormat
    if curPathDifferent then
        expected.BackStack <- (createPath "other", 5) :: expected.BackStack
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
    model.Path <- createPath "other"
    model.UndoStack <- undoAction :: model.UndoStack
    contr.Undo model |> Async.RunSynchronously

    let expected = createModel()
    expected.Path <- createPath "other"
    expected.Status <- Some <| MainStatus.cannotUndoDelete permanent deletedNode
    assertAreEqual expected model

