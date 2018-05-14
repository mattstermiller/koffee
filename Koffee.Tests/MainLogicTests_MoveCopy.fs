module Koffee.MainLogicTests_MoveCopy

open System
open NUnit.Framework
open FsUnitTyped
open Testing
open KellermanSoftware.CompareNetObjects

let nodeSameFolder = createNode "/c/path/file 2"
let nodeDiffFolder = createNode "/c/other/file 2"

let oldNodes = [
    createNode "/c/path/file 1"
    createNode "/c/path/file 3"
]

let newNodes = [
    createNode "/c/path/file 1"
    nodeSameFolder
]

let nodeCopy num =
    createNode ("/c/path/" + (MainLogic.Action.getCopyName "file 2" num))

let modelPathNode = createNode "/c/path"

let createModel () =
    let model = createBaseTestModel()
    model.Path <- modelPathNode.Path
    model.Nodes <- oldNodes
    model.Cursor <- 0
    model

let mockGetNodeFunc nodeFunc path =
    if path = modelPathNode.Path then Ok (Some modelPathNode)
    else Ok (nodeFunc path)

let mockGetNode nodeToReturn = mockGetNodeFunc (fun _ -> nodeToReturn)

let ex = UnauthorizedAccessException()

[<TestCase(false, false)>]
[<TestCase(false, true)>]
[<TestCase(true, false)>]
[<TestCase(true, true)>]
let ``Put item to move or copy in different folder with item of same name prompts for overwrite`` doCopy existingHidden =
    let src = nodeDiffFolder
    let dest = { nodeSameFolder with IsHidden = existingHidden }
    let config = Config()
    let getNode = mockGetNode (Some dest)
    let move _ _ = failwith "move should not be called"
    let copy _ _ = failwith "copy should not be called"
    let mutable openedHidden = None
    let openPath sh p s (model: MainModel) =
        model.Path <- p
        model.Nodes <- newNodes
        if s = SelectName (dest.Name) then
            model.Cursor <- 2
        openedHidden <- Some sh
        Ok ()
    let action = if doCopy then Copy else Move
    let item = Some (src, action)
    let model = createModel()
    model.YankRegister <- item
    let res = MainLogic.Action.put config getNode move copy openPath false model |> Async.RunSynchronously

    res |> shouldEqual (Ok ())
    let expected = createModel()
    expected.YankRegister <- item
    expected.Nodes <- newNodes
    expected.Cursor <- 2
    expected.InputMode <- Some (Confirm (Overwrite (action, src, dest)))
    assertAreEqual expected model
    openedHidden |> shouldEqual (Some existingHidden)
    config.ShowHidden |> shouldEqual false

[<TestCase(false)>]
[<TestCase(true)>]
let ``Put item to move or copy returns error`` doCopy =
    let src = nodeDiffFolder
    let dest = nodeSameFolder
    let config = Config()
    let getNode = mockGetNode None
    let move _ _ = if not doCopy then raise ex else failwith "move should not be called"
    let copy _ _ = if doCopy then raise ex else failwith "copy should not be called"
    let openPath _ p _ (model: MainModel) =
        model.Path <- p
        model.Nodes <- newNodes
        Ok ()
    let item = Some (src, if doCopy then Copy else Move)
    let model = createModel()
    model.YankRegister <- item
    let res = MainLogic.Action.put config getNode move copy openPath false model |> Async.RunSynchronously

    let expectedAction = if doCopy then CopiedItem (src, dest.Path) else MovedItem (src, dest.Path)
    res |> shouldEqual (Error (ItemActionError (expectedAction, model.PathFormat, ex)))
    let expected = createModel()
    expected.YankRegister <- item
    CompareLogic() |> ignoreMembers ["Status"] |> assertAreEqualWith expected model

// move tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Put item to move in different folder calls file sys move`` (overwrite: bool) =
    let src = nodeDiffFolder
    let dest = nodeSameFolder
    let config = Config()
    let getNode = mockGetNode (if overwrite then Some dest else None)
    let mutable moved = None
    let move s d = moved <- Some (s, d)
    let copy _ _ = failwith "copy should not be called"
    let openPath _ p _ (model: MainModel) =
        model.Path <- p
        model.Nodes <- newNodes
        Ok ()
    let model = createModel()
    model.YankRegister <- Some (src, Move)
    let res = MainLogic.Action.put config getNode move copy openPath overwrite model |> Async.RunSynchronously

    res |> shouldEqual (Ok ())
    moved |> shouldEqual (Some (src.Path, dest.Path))
    let expectedAction = MovedItem (src, dest.Path)
    let expected = createModel()
    expected.Nodes <- newNodes
    expected.UndoStack <- expectedAction :: expected.UndoStack
    expected.RedoStack <- []
    expected.Status <- Some <| MainStatus.actionComplete expectedAction model.PathFormat
    assertAreEqual expected model

[<Test>]
let ``Put item to move in same folder returns error``() =
    let src = nodeSameFolder
    let config = Config()
    let getNode = mockGetNode None
    let move _ _ = failwith "move should not be called"
    let copy _ _ = failwith "copy should not be called"
    let openPath _ p _ (model: MainModel) =
        model.Path <- p
        model.Nodes <- newNodes
        Ok ()
    let item = Some (src, Move)
    let model = createModel()
    model.YankRegister <- item
    let res = MainLogic.Action.put config getNode move copy openPath false model |> Async.RunSynchronously

    res |> shouldEqual (Error CannotMoveToSameFolder)
    let expected = createModel()
    expected.YankRegister <- item
    assertAreEqual expected model

// undo move tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo move item moves it back`` curPathDifferent =
    let prevNode = newNodes.[1]
    let curNode = oldNodes.[1]
    let getNode _ = Ok None
    let mutable moved = None
    let move s d = moved <- Some (s, d)
    let mutable selected = None
    let openPath p s (model: MainModel) =
        model.Nodes <- newNodes
        model.Path <- p
        model.Status <- None
        selected <- Some s
        Ok ()
    let model = createModel()
    if curPathDifferent then
        model.Path <- createPath "/c/other"
    let res = MainLogic.Action.undoMove getNode move openPath prevNode curNode.Path model |> Async.RunSynchronously

    res |> shouldEqual (Ok ())
    moved |> shouldEqual (Some (curNode.Path, prevNode.Path))
    selected |> shouldEqual (Some (SelectName prevNode.Name))
    let expectedAction = MovedItem (prevNode, curNode.Path)
    let expected = createModel()
    expected.Nodes <- newNodes
    assertAreEqual expected model

[<Test>]
let ``Undo move item when previous path is occupied returns error``() =
    let prevNode = newNodes.[1]
    let curNode = oldNodes.[1]
    let getNode p = if p = prevNode.Path then Ok (Some prevNode) else Ok None
    let move _ _ = failwith "move should not be called"
    let openPath _ _ _ = failwith "openPath should not be called"
    let model = createModel()
    model.Path <- createPath "/c/other"
    let res = MainLogic.Action.undoMove getNode move openPath prevNode curNode.Path model |> Async.RunSynchronously

    res |> shouldEqual (Error (CannotUndoMoveToExisting prevNode))
    let expected = createModel()
    expected.Path <- createPath "/c/other"
    assertAreEqual expected model

[<Test>]
let ``Undo move item handles move error by returning error``() =
    let prevNode = newNodes.[1]
    let curNode = oldNodes.[1]
    let getNode _ = Ok None
    let move _ _ = raise ex
    let openPath _ _ _ = failwith "openPath should not be called"
    let model = createModel()
    model.Path <- createPath "/c/other"
    let res = MainLogic.Action.undoMove getNode move openPath prevNode curNode.Path model |> Async.RunSynchronously

    let expectedAction = MovedItem (curNode, prevNode.Path)
    res |> shouldEqual (Error (ItemActionError (expectedAction, model.PathFormat, ex)))
    let expected = createModel()
    expected.Path <- createPath "/c/other"
    CompareLogic() |> ignoreMembers ["Status"] |> assertAreEqualWith expected model

// copy tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Put item to copy in different folder calls file sys copy`` (overwrite: bool) =
    let src = nodeDiffFolder
    let dest = nodeSameFolder
    let config = Config()
    let getNode = mockGetNode (if overwrite then Some dest else None)
    let move _ _ = failwith "move should not be called"
    let mutable copied = None
    let copy s d = copied <- Some (s, d)
    let openPath _ p _ (model: MainModel) =
        model.Path <- p
        model.Nodes <- newNodes
        Ok ()
    let model = createModel()
    model.YankRegister <- Some (src, Copy)
    let res = MainLogic.Action.put config getNode move copy openPath overwrite model |> Async.RunSynchronously

    res |> shouldEqual (Ok ())
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
    let getNode = mockGetNodeFunc (fun p -> if existingPaths |> List.contains p then Some src else None)
    let move _ _ = failwith "move should not be called"
    let mutable copied = None
    let copy s d = copied <- Some (s, d)
    let openPath _ p _ (m: MainModel) =
        m.Path <- p
        m.Nodes <- newNodes
        Ok ()
    let model = createModel()
    model.YankRegister <- Some (src, Copy)
    let res = MainLogic.Action.put config getNode move copy openPath false model |> Async.RunSynchronously

    res |> shouldEqual (Ok ())
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

// undo copy tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo copy item when copy has same timestamp deletes copy`` curPathDifferent =
    let modified = Some (DateTime(2000, 1, 1))
    let original = { nodeDiffFolder with Modified = modified }
    let copied = { nodeSameFolder with Modified = modified }
    let getNode p = if p = copied.Path then Ok (Some copied) else Ok None
    let model = createModel()
    let mutable deleted = None
    let delete p =
        deleted <- Some p
        model.Status <- None
        Ok ()
    let recycle _ = failwith "recycle should not be called"
    let refresh (model: MainModel) = model.Nodes <- newNodes
    if curPathDifferent then
        model.Path <- createPath "/c/other"
    let res = MainLogic.Action.undoCopy getNode delete recycle refresh original copied.Path model |> Async.RunSynchronously

    res |> shouldEqual (Ok ())
    deleted |> shouldEqual (Some copied.Path)
    let expected = createModel()
    if curPathDifferent then
        expected.Path <- createPath "/c/other"
    else
        expected.Nodes <- newNodes
    assertAreEqual expected model

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo copy item when copy has different or no timestamp recycles copy`` hasTimestamp =
    let time = if hasTimestamp then Some (DateTime(2000, 1, 1)) else None
    let original = { nodeDiffFolder with Modified = time }
    let copied = { nodeSameFolder with Modified = time |> Option.map (fun t -> t.AddDays(1.0)) }
    let getNode p = if p = copied.Path then Ok (Some copied) else Ok None
    let delete _ = failwith "delete should not be called"
    let model = createModel()
    let mutable recycled = None
    let recycle p =
        recycled <- Some p
        model.Status <- None
        Ok ()
    let refresh (model: MainModel) = model.Nodes <- newNodes
    let res = MainLogic.Action.undoCopy getNode delete recycle refresh original copied.Path model |> Async.RunSynchronously

    res |> shouldEqual (Ok ())
    recycled |> shouldEqual (Some copied.Path)
    let expected = createModel()
    expected.Nodes <- newNodes
    assertAreEqual expected model

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo copy item handles errors by returning error and consuming action`` throwOnGetNode =
    let original = nodeDiffFolder
    let copied = nodeSameFolder
    let action = DeletedItem (copied, false)
    let model = createModel()
    let error = ItemActionError (action, model.PathFormat, ex)
    let getNode _ = if throwOnGetNode then Error error else Ok None
    let fsFunc _ = Error ex
    let refresh _ = failwith "refresh should not be called"
    let res = MainLogic.Action.undoCopy getNode fsFunc fsFunc refresh original copied.Path model |> Async.RunSynchronously

    res |> shouldEqual (Error error)
    let expected = createModel()
    CompareLogic() |> ignoreMembers ["Status"] |> assertAreEqualWith expected model
