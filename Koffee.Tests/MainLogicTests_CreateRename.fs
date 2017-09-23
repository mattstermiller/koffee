module Koffee.MainLogicTests_CreateRename

open NUnit.Framework
open FsUnitTyped
open Testing

let oldNodes = [
    createNode "path" "one"
    createNode "path" "two"
]

let newNodes = [
    createNode "path" "new one"
    createNode "path" "new two"
]

let createModel () =
    let model = createBaseTestModel()
    model.Path <- createPath "path"
    model.Nodes <- oldNodes
    model.Cursor <- 0
    model.CommandText <- ""
    model.CommandTextSelection <- (1, 1)
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
    let createNode = newNodes.[1]
    let model = createModel()
    MainLogic.Action.create getNode create openPath Folder createNode.Name model

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
let ``Create folder sets error status when item already exists at path`` existingHidden =
    let existing = { oldNodes.[1] with IsHidden = existingHidden }
    let getNode _ = Some existing
    let create _ _ = failwith "create should not be called"
    let mutable selected = None
    let openPath p s (model: MainModel) =
        model.Path <- p
        selected <- Some s
    let createNode = newNodes.[1]
    let model = createModel()
    MainLogic.Action.create getNode create openPath Folder createNode.Name model

    let expected = createModel()
    expected.Status <- Some <| MainStatus.cannotCreateAlreadyExists Folder createNode.Name existingHidden
    assertAreEqual expected model
    selected |> shouldEqual (Some (SelectName existing.Name))

[<Test>]
let ``Create folder handles error by setting error status``() =
    let getNode _ = None
    let create _ _ = raise ex
    let openPath _ _ _ = failwith "openPath should not be called"
    let createNode = newNodes.[1]
    let model = createModel()
    MainLogic.Action.create getNode create openPath Folder createNode.Name model

    let expected = createModel()
    expected |> MainStatus.setActionExceptionStatus (CreatedItem createNode) ex
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
    let refresh (model: MainModel) =
        model.Nodes <- newNodes
    let createdNode = oldNodes.[1]
    if curPathDifferent then
        model.Path <- createPath "other"
    MainLogic.Action.undoCreate isEmpty delete refresh createdNode model |> Async.RunSynchronously

    deleted |> shouldEqual (Some createdNode.Path)
    let expected = createModel()
    if curPathDifferent then
        expected.Path <- createPath "other"
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

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo create handles error by setting error status`` isEmptyThrows =
    let isEmpty _ = if isEmptyThrows then raise ex else true
    let delete _ = if not isEmptyThrows then raise ex else failwith "delete should not be called"
    let refresh _ = failwith "refresh should not be called"
    let createdNode = oldNodes.[1]
    let model = createModel()
    MainLogic.Action.undoCreate isEmpty delete refresh createdNode model |> Async.RunSynchronously

    let expected = createModel()
    expected |> MainStatus.setActionExceptionStatus (DeletedItem (createdNode, true)) ex
    assertAreEqual expected model

// rename tests

[<Test>]
let ``Rename calls file sys move, openPath and sets status``() =
    let mutable renamed = None
    let move s d = renamed <- Some (s, d)
    let openPath p _ (model: MainModel) =
        model.Path <- p
        model.Nodes <- newNodes
    let newName = newNodes.[1].Name
    let model = createModel()
    MainLogic.Action.rename move openPath oldNodes.[1] newName model

    renamed |> shouldEqual (Some (oldNodes.[1].Path, newNodes.[1].Path))
    let expectedAction = RenamedItem (oldNodes.[1], newName)
    let expected = createModel()
    expected.Nodes <- newNodes
    expected.UndoStack <- expectedAction :: expected.UndoStack
    expected.RedoStack <- []
    expected.Status <- Some <| MainStatus.actionComplete expectedAction model.PathFormat
    assertAreEqual expected model

[<Test>]
let ``Rename handles error by setting error status``() =
    let move _ _ = raise ex
    let openPath _ _ _ = failwith "openPath should not be called"
    let newName = newNodes.[1].Name
    let model = createModel()
    MainLogic.Action.rename move openPath oldNodes.[1] newName model

    let expectedAction = RenamedItem (oldNodes.[1], newNodes.[1].Name)
    let expected = createModel()
    expected |> MainStatus.setActionExceptionStatus expectedAction ex
    assertAreEqual expected model

// undo rename tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo rename item names file back to original`` curPathDifferent =
    let mutable moved = None
    let move s d = moved <- Some (s, d)
    let mutable selected = None
    let openPath p select (model: MainModel) =
        model.Path <- p
        model.Nodes <- newNodes
        selected <- Some select
    let prevNode = newNodes.[1]
    let curNode = oldNodes.[1]
    let model = createModel()
    if curPathDifferent then
        model.Path <- createPath "other"
    MainLogic.Action.undoRename move openPath prevNode curNode.Name model

    moved |> shouldEqual (Some (curNode.Path, prevNode.Path))
    selected |> shouldEqual (Some (SelectName prevNode.Name))
    let expected = createModel()
    expected.Nodes <- newNodes
    assertAreEqual expected model

[<Test>]
let ``Undo rename item handles error by setting error status``() =
    let move _ _ = raise ex
    let openPath _ _ _ = failwith "openPath should not be called"
    let prevNode = newNodes.[1]
    let curNode = oldNodes.[1]
    let model = createModel()
    MainLogic.Action.undoRename move openPath prevNode curNode.Name model

    let expectedAction = RenamedItem (curNode, prevNode.Name)
    let expected = createModel()
    expected |> MainStatus.setActionExceptionStatus expectedAction ex
    assertAreEqual expected model

// start rename selection tests

let renameTextSelection cursorPosition fileName =
    let node = createNode "path" fileName
    let model = createModel()
    model.Nodes <- List.append oldNodes [node]
    model.Cursor <- model.Nodes.Length - 1
    MainLogic.Action.startInput (Rename cursorPosition) model

    model.CommandInputMode |> shouldEqual (Some (Rename cursorPosition))
    model.CommandText |> shouldEqual node.Name
    model.CommandTextSelection

[<Test>]
let ``StartInput for rename at beginning sets command text and selection``() =
    renameTextSelection Begin "three.txt.old" |> shouldEqual (0, 0)

[<Test>]
let ``StartInput for rename at end of name sets command text and selection``() =
    renameTextSelection EndName "three.txt.old" |> shouldEqual (9, 0)

[<Test>]
let ``StartInput for rename at end of full name sets command text and selection``() =
    renameTextSelection End "three.txt.old" |> shouldEqual (13, 0)

[<Test>]
let ``StartInput for rename replace name sets command text and selection``() =
    renameTextSelection ReplaceName "three.txt.old" |> shouldEqual (0, 9)

[<Test>]
let ``StartInput for rename replace name with no name sets command text and selection``() =
    renameTextSelection ReplaceName ".txt" |> shouldEqual (0, 0)

[<Test>]
let ``StartInput for rename replace all sets command text and selection``() =
    renameTextSelection ReplaceAll "three.txt.old" |> shouldEqual (0, 13)

[<Test>]
let ``StartInput for rename replace all with no extension sets command text and selection``() =
    renameTextSelection ReplaceAll "three" |> shouldEqual (0, 5)

[<Test>]
let ``StartInput for rename replace all with just dot sets command text and selection``() =
    renameTextSelection ReplaceAll "three." |> shouldEqual (0, 6)
