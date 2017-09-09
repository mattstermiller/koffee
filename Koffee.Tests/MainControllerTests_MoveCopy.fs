module Koffee.MainControllerTests_MoveCopy

open System
open System.Windows.Input
open FSharp.Desktop.UI
open NUnit.Framework
open FsUnitTyped
open Foq
open KellermanSoftware.CompareNetObjects
open Testing

let nodeSameFolder = createNode "path" "file 2"
let nodeDiffFolder = createNode "other" "file 2"

let oldNodes = [
    createNode "path" "file 1"
    createNode "path" "file 3"
]

let nodeCopy num =
    createNode "path" (MainLogic.Action.getCopyName "file 2" num)

let newNodes = [
    createNode "path" "file 1"
    nodeSameFolder
    nodeCopy 1
    nodeCopy 2
    nodeCopy 0
    createNode "path" "file 3"
]

let createModel () =
    let model = createBaseTestModel()
    model.Path <- createPath "path"
    model.Nodes <- oldNodes
    model.Cursor <- 0
    model

let ex = UnauthorizedAccessException()

[<TestCase(false, false)>]
[<TestCase(false, true)>]
[<TestCase(true, false)>]
[<TestCase(true, true)>]
let ``Put item to move or copy in different folder with item of same name prompts for overwrite`` doCopy existingHidden =
    let src = nodeDiffFolder
    let dest = { nodeSameFolder with IsHidden = existingHidden }
    let config = Config()
    let getNode _ = Some dest
    let move _ _ = failwith "move should not be called"
    let copy _ _ = failwith "copy should not be called"
    let openPath p s (m: MainModel) =
        m.Nodes <- newNodes
        if s = SelectName (dest.Name) then
            m.Cursor <- 2
        if existingHidden && not config.ShowHidden then
            failwith "config should be temporarily be set to show hidden, but was not"
    let item = Some (src, if doCopy then Copy else Move)
    let model = createModel()
    model.ItemBuffer <- item
    MainLogic.Action.put config getNode move copy openPath false model |> Async.RunSynchronously

    let expected = createModel()
    expected.ItemBuffer <- item
    expected.Nodes <- newNodes
    expected.Cursor <- 2
    expected.CommandInputMode <- Some (Confirm Overwrite)
    assertAreEqual expected model
    config.ShowHidden |> shouldEqual false

[<TestCase(false)>]
[<TestCase(true)>]
let ``Put item to move or copy handles error by setting error status`` doCopy =
    let src = nodeDiffFolder
    let dest = nodeSameFolder
    let config = Config()
    let getNode _ = None
    let move _ _ = if not doCopy then raise ex else failwith "move should not be called"
    let copy _ _ = if doCopy then raise ex else failwith "copy should not be called"
    let openPath p s (m: MainModel) = m.Nodes <- newNodes
    let item = Some (src, if doCopy then Copy else Move)
    let model = createModel()
    model.ItemBuffer <- item
    MainLogic.Action.put config getNode move copy openPath false model |> Async.RunSynchronously

    let expectedAction = if doCopy then CopiedItem (src, dest.Path) else MovedItem (src, dest.Path)
    let expected = createModel()
    expected.ItemBuffer <- item
    expected |> MainStatus.setActionExceptionStatus expectedAction ex
    assertAreEqual expected model

// move tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Put item to move in different folder calls file sys move`` (overwrite: bool) =
    let src = nodeDiffFolder
    let dest = nodeSameFolder
    let config = Config()
    let getNode _ = if overwrite then Some dest else None
    let mutable moved = None
    let move s d = moved <- Some (s, d)
    let copy _ _ = failwith "copy should not be called"
    let openPath p _ (m: MainModel) = m.Nodes <- newNodes
    let model = createModel()
    model.ItemBuffer <- Some (src, Move)
    MainLogic.Action.put config getNode move copy openPath overwrite model |> Async.RunSynchronously

    moved |> shouldEqual (Some (src.Path, dest.Path))
    let expectedAction = MovedItem (src, dest.Path)
    let expected = createModel()
    expected.Nodes <- newNodes
    expected.UndoStack <- expectedAction :: expected.UndoStack
    expected.RedoStack <- []
    expected.Status <- Some <| MainStatus.actionComplete expectedAction model.PathFormat
    assertAreEqual expected model

[<Test>]
let ``Put item to move in same folder gives same-folder message``() =
    let src = nodeSameFolder
    let config = Config()
    let getNode _ = None
    let move _ _ = failwith "move should not be called"
    let copy _ _ = failwith "copy should not be called"
    let openPath p s (m: MainModel) = m.Nodes <- newNodes
    let item = Some (src, Move)
    let model = createModel()
    model.ItemBuffer <- item
    MainLogic.Action.put config getNode move copy openPath false model |> Async.RunSynchronously

    let expected = createModel()
    expected.ItemBuffer <- item
    expected.Status <- Some <| MainStatus.cannotMoveToSameFolder
    assertAreEqual expected model

// copy tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Put item to copy in different folder calls file sys copy`` (overwrite: bool) =
    let src = nodeDiffFolder
    let dest = nodeSameFolder
    let config = Config()
    let getNode _ = if overwrite then Some dest else None
    let move _ _ = failwith "move should not be called"
    let mutable copied = None
    let copy s d = copied <- Some (s, d)
    let openPath p _ (m: MainModel) = m.Nodes <- newNodes
    let model = createModel()
    model.ItemBuffer <- Some (src, Copy)
    MainLogic.Action.put config getNode move copy openPath overwrite model |> Async.RunSynchronously

    copied |> shouldEqual (Some (src.Path, dest.Path))
    let expectedAction = CopiedItem (src, dest.Path)
    let expected = createModel()
    expected.Nodes <- newNodes
    expected.UndoStack <- expectedAction :: expected.UndoStack
    expected.RedoStack <- []
    expected.Status <- Some <| MainStatus.actionComplete expectedAction model.PathFormat
    assertAreEqual expected model

[<TestCase(0)>]
[<TestCase(1)>]
[<TestCase(2)>]
let ``Put item to copy in same folder calls file sys copy with new name`` existingCopies =
    let src = nodeSameFolder
    let config = Config()
    let existingPaths = List.init existingCopies (fun i -> (nodeCopy i).Path)
    let getNode p = if existingPaths |> List.contains p then Some src else None
    let move _ _ = failwith "move should not be called"
    let mutable copied = None
    let copy s d = copied <- Some (s, d)
    let openPath p _ (m: MainModel) = m.Nodes <- newNodes
    let model = createModel()
    model.ItemBuffer <- Some (src, Copy)
    MainLogic.Action.put config getNode move copy openPath false model |> Async.RunSynchronously

    let destName = MainLogic.Action.getCopyName src.Name existingCopies
    let destPath = model.Path.Join destName
    copied |> shouldEqual (Some (src.Path, destPath))
    let expectedAction = CopiedItem (nodeSameFolder, destPath)
    let expected = createModel()
    expected.Nodes <- newNodes
    expected.UndoStack <- expectedAction :: expected.UndoStack
    expected.RedoStack <- []
    expected.Status <- Some <| MainStatus.actionComplete expectedAction model.PathFormat
    assertAreEqual expected model
