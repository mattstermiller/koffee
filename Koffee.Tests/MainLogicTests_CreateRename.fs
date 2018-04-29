module Koffee.MainLogicTests_CreateRename

open NUnit.Framework
open FsUnitTyped
open Testing

let oldNodes = [
    createNode "/c/path/one"
    createNode "/c/path/two"
]

let newNodes = [
    createNode "/c/path/new one"
    createNode "/c/path/new two"
]

let createModel () =
    let model = createBaseTestModel()
    model.Path <- createPath "/c/path"
    model.Nodes <- oldNodes
    model.Cursor <- 0
    model.InputText <- ""
    model.InputTextSelection <- (1, 1)
    model

let ex = System.UnauthorizedAccessException()

// create tests

[<Test>]
let ``Create folder calls file sys create, openPath and sets status``() =
    let getNode _ = None
    let mutable created = None
    let create nodeType path = created <- Some (nodeType, path)
    let mutable selected = None
    let openPath p s (model: MainModel) =
        model.Path <- p
        model.Nodes <- newNodes
        model.Cursor <- 1
        selected <- Some s
        Ok ()
    let createNode = newNodes.[1]
    let model = createModel()

    MainLogic.Action.create getNode create openPath Folder createNode.Name model
    |> shouldEqual (Ok ())

    created |> shouldEqual (Some (createNode.Type, createNode.Path))
    let expectedAction = CreatedItem createNode
    let expected = createModel()
    expected.Nodes <- newNodes
    expected.Cursor <- 1
    expected.UndoStack <- expectedAction :: expected.UndoStack
    expected.RedoStack <- []
    expected.Status <- Some <| MainStatus.actionComplete expectedAction model.PathFormat
    assertAreEqual expected model
    selected |> shouldEqual (Some (SelectName createNode.Name))

[<TestCase(false)>]
[<TestCase(true)>]
let ``Create folder returns error when item already exists at path`` existingHidden =
    let existing = { oldNodes.[1] with IsHidden = existingHidden }
    let getNode _ = Some existing
    let create _ _ = failwith "create should not be called"
    let mutable selected = None
    let openPath p s (model: MainModel) =
        model.Path <- p
        selected <- Some s
        Ok ()
    let createNode = newNodes.[1]
    let model = createModel()

    let res = MainLogic.Action.create getNode create openPath Folder createNode.Name model

    res |> shouldEqual (Error (CannotUseNameAlreadyExists ("create", Folder, createNode.Name, existingHidden)))
    let expected = createModel()
    assertAreEqual expected model
    selected |> shouldEqual (Some (SelectName existing.Name))

[<Test>]
let ``Create folder handles error by returning error``() =
    let getNode _ = None
    let create _ _ = raise ex
    let openPath _ _ _ = failwith "openPath should not be called"
    let createNode = newNodes.[1]
    let model = createModel()

    let res = MainLogic.Action.create getNode create openPath Folder createNode.Name model

    res |> shouldEqual (Error (ItemActionError ((CreatedItem createNode), model.PathFormat, ex)))
    let expected = createModel()
    assertAreEqual expected model

// undo create tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo create empty node calls delete`` curPathDifferent =
    let isEmpty _ = true
    let model = createModel()
    let mutable deleted = None
    let delete p =
        deleted <- Some p
        model.Status <- None
        Ok ()
    let refresh (model: MainModel) =
        model.Nodes <- newNodes
    let createdNode = oldNodes.[1]
    if curPathDifferent then
        model.Path <- createPath "/c/other"
    MainLogic.Action.undoCreate isEmpty delete refresh createdNode model |> Async.RunSynchronously

    deleted |> shouldEqual (Some createdNode.Path)
    let expected = createModel()
    if curPathDifferent then
        expected.Path <- createPath "/c/other"
    else
        expected.Nodes <- newNodes
    assertAreEqual expected model

[<Test>]
let ``Undo create non-empty node sets status only``() =
    let isEmpty _ = false
    let delete _ = failwith "delete should not be called"
    let refresh _ = failwith "refresh should not be called"
    let createdNode = oldNodes.[1]
    let model = createModel()
    MainLogic.Action.undoCreate isEmpty delete refresh createdNode model |> Async.RunSynchronously

    let expected = createModel()
    expected.Status <- Some <| MainStatus.cannotUndoNonEmptyCreated createdNode
    assertAreEqual expected model

[<Test>]
let ``Undo create handles error by setting error status`` () =
    let isEmpty _ = true
    let delete _ = Error ex
    let refresh _ = failwith "refresh should not be called"
    let createdNode = oldNodes.[1]
    let model = createModel()
    MainLogic.Action.undoCreate isEmpty delete refresh createdNode model |> Async.RunSynchronously

    let expected = createModel()
    expected.SetItemError (DeletedItem (createdNode, true)) ex
    assertAreEqual expected model

// rename tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Rename calls file sys move, openPath and sets status`` diffCaseOnly =
    let currentNode = oldNodes.[1]
    let renamedNode = if diffCaseOnly then currentNode else newNodes.[1]
    let getNode _ = if diffCaseOnly then Some currentNode else None
    let mutable renamed = None
    let move s d = renamed <- Some (s, d)
    let openPath p _ (model: MainModel) =
        model.Path <- p
        model.Nodes <- newNodes
        Ok ()
    let model = createModel()

    MainLogic.Action.rename getNode move openPath currentNode renamedNode.Name model
    |> shouldEqual (Ok ())

    renamed |> shouldEqual (Some (currentNode.Path, renamedNode.Path))
    let expectedAction = RenamedItem (currentNode, renamedNode.Name)
    let expected = createModel()
    expected.Nodes <- newNodes
    expected.UndoStack <- expectedAction :: expected.UndoStack
    expected.RedoStack <- []
    expected.Status <- Some <| MainStatus.actionComplete expectedAction model.PathFormat
    assertAreEqual expected model

[<TestCase(false)>]
[<TestCase(true)>]
let ``Rename to path with existing item returns error`` existingHidden =
    let currentNode = oldNodes.[1]
    let renamedNode = { newNodes.[1] with IsHidden = existingHidden }
    let getNode p = if p = renamedNode.Path then Some renamedNode else None
    let move _ _ = failwith "move should not be called"
    let openPath _ _ _ = failwith "openPath should not be called"
    let model = createModel()

    let res = MainLogic.Action.rename getNode move openPath currentNode renamedNode.Name model

    res |> shouldEqual (Error (CannotUseNameAlreadyExists ("rename", Folder, renamedNode.Name, existingHidden)))
    let expected = createModel()
    assertAreEqual expected model

[<Test>]
let ``Rename handles error by returning error``() =
    let currentNode = oldNodes.[1]
    let renamedNode = newNodes.[1]
    let getNode _ = None
    let move _ _ = raise ex
    let openPath _ _ _ = failwith "openPath should not be called"
    let model = createModel()

    let res = MainLogic.Action.rename getNode move openPath currentNode renamedNode.Name model

    let expectedAction = RenamedItem (currentNode, renamedNode.Name)
    res |> shouldEqual (Error (ItemActionError (expectedAction, model.PathFormat, ex)))
    let expected = createModel()
    assertAreEqual expected model

// undo rename tests

[<TestCase(false, false)>]
[<TestCase(true, false)>]
[<TestCase(false, true)>]
[<TestCase(true, true)>]
let ``Undo rename item names file back to original`` curPathDifferent diffCaseOnly =
    let prevNode = newNodes.[1]
    let curNode = if diffCaseOnly then prevNode else oldNodes.[1]
    let getNode _ = if diffCaseOnly then Some curNode else None
    let mutable moved = None
    let move s d = moved <- Some (s, d)
    let mutable selected = None
    let openPath p select (model: MainModel) =
        model.Path <- p
        model.Nodes <- newNodes
        selected <- Some select
    let model = createModel()
    if curPathDifferent then
        model.Path <- createPath "/c/other"
    MainLogic.Action.undoRename getNode move openPath prevNode curNode.Name model

    moved |> shouldEqual (Some (curNode.Path, prevNode.Path))
    selected |> shouldEqual (Some (SelectName prevNode.Name))
    let expected = createModel()
    expected.Nodes <- newNodes
    assertAreEqual expected model

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo rename to path with existing item sets error status`` existingHidden =
    let prevNode = { newNodes.[1] with IsHidden = existingHidden }
    let curNode = oldNodes.[1]
    let getNode p = if p = prevNode.Path then Some prevNode else None
    let move _ _ = failwith "move should not be called"
    let openPath _ _ _ = failwith "openPath should not be called"
    let model = createModel()
    MainLogic.Action.undoRename getNode move openPath prevNode curNode.Name model

    let expected = createModel()
    expected.SetError <| CannotUseNameAlreadyExists ("rename", Folder, prevNode.Name, existingHidden)
    assertAreEqual expected model

[<Test>]
let ``Undo rename item handles error by setting error status``() =
    let getNode _ = None
    let move _ _ = raise ex
    let openPath _ _ _ = failwith "openPath should not be called"
    let prevNode = newNodes.[1]
    let curNode = oldNodes.[1]
    let model = createModel()
    MainLogic.Action.undoRename getNode move openPath prevNode curNode.Name model

    let expectedAction = RenamedItem (curNode, prevNode.Name)
    let expected = createModel()
    expected.SetItemError expectedAction ex
    assertAreEqual expected model

// start rename selection tests

let renameTextSelection cursorPosition fileName =
    let node = createNode ("/c/path/" + fileName)
    let model = createModel()
    model.Nodes <- List.append oldNodes [node]
    model.Cursor <- model.Nodes.Length - 1
    let inputMode = Input (Rename cursorPosition)
    let getNode _ = None
    MainLogic.Action.startInput getNode inputMode model
    |> shouldEqual (Ok ())

    model.InputMode |> shouldEqual (Some inputMode)
    model.InputText |> shouldEqual node.Name
    model.InputTextSelection

[<Test>]
let ``StartInput for rename at beginning sets InputText and selection``() =
    renameTextSelection Begin "three.txt.old" |> shouldEqual (0, 0)

[<Test>]
let ``StartInput for rename at end of name sets InputText and selection``() =
    renameTextSelection EndName "three.txt.old" |> shouldEqual (9, 0)

[<Test>]
let ``StartInput for rename at end of full name sets InputText and selection``() =
    renameTextSelection End "three.txt.old" |> shouldEqual (13, 0)

[<Test>]
let ``StartInput for rename replace name sets InputText and selection``() =
    renameTextSelection ReplaceName "three.txt.old" |> shouldEqual (0, 9)

[<Test>]
let ``StartInput for rename replace name with no name sets InputText and selection``() =
    renameTextSelection ReplaceName ".txt" |> shouldEqual (0, 0)

[<Test>]
let ``StartInput for rename replace all sets InputText and selection``() =
    renameTextSelection ReplaceAll "three.txt.old" |> shouldEqual (0, 13)

[<Test>]
let ``StartInput for rename replace all with no extension sets InputText and selection``() =
    renameTextSelection ReplaceAll "three" |> shouldEqual (0, 5)

[<Test>]
let ``StartInput for rename replace all with just dot sets InputText and selection``() =
    renameTextSelection ReplaceAll "three." |> shouldEqual (0, 6)
