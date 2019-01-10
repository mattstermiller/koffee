module Koffee.MainLogicTests_CreateRename

open NUnit.Framework
open FsUnitTyped
open Testing
open KellermanSoftware.CompareNetObjects

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

let ex = System.UnauthorizedAccessException() :> exn

// create tests

[<Test>]
let ``Create folder calls file sys create, openPath and sets status``() =
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- fun _ -> Ok None
    fsReader.GetNodes <- fun _ _ -> Ok newNodes
    let fsWriter = FakeFileSystemWriter()
    let mutable created = None
    fsWriter.Create <- fun nodeType path ->
        created <- Some (nodeType, path)
        Ok ()
    let createNode = newNodes.[1]
    let model = createModel()

    MainLogic.Action.create_m fsReader fsWriter Folder createNode.Name model
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

[<TestCase(false)>]
[<TestCase(true)>]
let ``Create folder returns error when item already exists at path`` existingHidden =
    let existing = { oldNodes.[1] with IsHidden = existingHidden }
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- fun _ -> Ok <| Some existing
    fsReader.GetNodes <- fun _ _ -> Ok newNodes
    let fsWriter = FakeFileSystemWriter()
    let createNode = newNodes.[1]
    let model = createModel()

    let res = MainLogic.Action.create_m fsReader fsWriter Folder createNode.Name model

    res |> shouldEqual (Error (CannotUseNameAlreadyExists ("create", Folder, createNode.Name, existingHidden)))
    let expected = createModel()
    expected.Nodes <- newNodes
    expected.Cursor <- 0
    assertAreEqual expected model

[<Test>]
let ``Create folder handles error by returning error``() =
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- fun _ -> Ok None
    let fsWriter = FakeFileSystemWriter()
    fsWriter.Create <- fun _ _ -> Error ex
    let createNode = newNodes.[1]
    let model = createModel()

    let res = MainLogic.Action.create_m fsReader fsWriter Folder createNode.Name model

    res |> shouldEqual (Error (ItemActionError ((CreatedItem createNode), model.PathFormat, ex)))
    let expected = createModel()
    assertAreEqual expected model

// undo create tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo create empty node calls delete`` curPathDifferent =
    let fsReader = FakeFileSystemReader()
    fsReader.IsEmpty <- fun _ -> true
    fsReader.GetNodes <- fun _ _ -> Ok newNodes
    let model = createModel()
    let fsWriter = FakeFileSystemWriter()
    let mutable deleted = None
    fsWriter.Delete <- fun p ->
        deleted <- Some p
        Ok ()
    let createdNode = oldNodes.[1]
    if curPathDifferent then
        model.Path <- createPath "/c/other"
    let res = MainLogic.Action.undoCreate_m fsReader fsWriter createdNode model |> Async.RunSynchronously

    res |> shouldEqual (Ok ())
    deleted |> shouldEqual (Some createdNode.Path)
    let expected = createModel()
    if curPathDifferent then
        expected.Path <- createPath "/c/other"
    else
        expected.Nodes <- newNodes
        expected.Cursor <- 0
    CompareLogic() |> ignoreMembers ["Status"] |> assertAreEqualWith expected model

[<Test>]
let ``Undo create non-empty node returns error``() =
    let fsReader = FakeFileSystemReader()
    fsReader.IsEmpty <- fun _ -> false
    let fsWriter = FakeFileSystemWriter()
    let createdNode = oldNodes.[1]
    let model = createModel()
    let res = MainLogic.Action.undoCreate_m fsReader fsWriter createdNode model |> Async.RunSynchronously

    res |> shouldEqual (Error (CannotUndoNonEmptyCreated createdNode))
    let expected = createModel()
    assertAreEqual expected model

[<Test>]
let ``Undo create handles delete error by returning error`` () =
    let fsReader = FakeFileSystemReader()
    fsReader.IsEmpty <- fun _ -> true
    let fsWriter = FakeFileSystemWriter()
    fsWriter.Delete <- fun _ -> Error ex
    let createdNode = oldNodes.[1]
    let model = createModel()
    let res = MainLogic.Action.undoCreate_m fsReader fsWriter createdNode model |> Async.RunSynchronously

    res |> shouldEqual (Error (ItemActionError (DeletedItem (createdNode, true), model.PathFormat, ex)))
    let expected = createModel()
    CompareLogic() |> ignoreMembers ["Status"] |> assertAreEqualWith expected model

// rename tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Rename calls file sys move, openPath and sets status`` diffCaseOnly =
    let currentNode = oldNodes.[1]
    let renamedNode = if diffCaseOnly then currentNode else newNodes.[1]
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- fun _ -> if diffCaseOnly then Ok (Some currentNode) else Ok None
    fsReader.GetNodes <- fun _ _ -> Ok newNodes
    let fsWriter = FakeFileSystemWriter()
    let mutable renamed = None
    fsWriter.Move <- fun s d ->
        renamed <- Some (s, d)
        Ok ()
    let model = createModel()

    MainLogic.Action.rename_m fsReader fsWriter currentNode renamedNode.Name model
    |> shouldEqual (Ok ())

    renamed |> shouldEqual (Some (currentNode.Path, renamedNode.Path))
    let expectedAction = RenamedItem (currentNode, renamedNode.Name)
    let expected = createModel()
    expected.Nodes <- newNodes
    expected.Cursor <- if diffCaseOnly then 0 else 1
    expected.UndoStack <- expectedAction :: expected.UndoStack
    expected.RedoStack <- []
    expected.Status <- Some <| MainStatus.actionComplete expectedAction model.PathFormat
    assertAreEqual expected model

[<TestCase(false)>]
[<TestCase(true)>]
let ``Rename to path with existing item returns error`` existingHidden =
    let currentNode = oldNodes.[1]
    let renamedNode = { newNodes.[1] with IsHidden = existingHidden }
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- fun p -> if p = renamedNode.Path then Ok (Some renamedNode) else Ok None
    let fsWriter = FakeFileSystemWriter()
    let model = createModel()

    let res = MainLogic.Action.rename_m fsReader fsWriter currentNode renamedNode.Name model

    res |> shouldEqual (Error (CannotUseNameAlreadyExists ("rename", Folder, renamedNode.Name, existingHidden)))
    let expected = createModel()
    assertAreEqual expected model

[<Test>]
let ``Rename handles error by returning error``() =
    let currentNode = oldNodes.[1]
    let renamedNode = newNodes.[1]
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- fun _ -> Ok None
    let fsWriter = FakeFileSystemWriter()
    fsWriter.Move <- fun _ _ -> Error ex
    let model = createModel()

    let res = MainLogic.Action.rename_m fsReader fsWriter currentNode renamedNode.Name model

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
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- fun _ -> if diffCaseOnly then Ok (Some curNode) else Ok None
    fsReader.GetNodes <- fun _ _ -> Ok newNodes
    let fsWriter = FakeFileSystemWriter()
    let mutable moved = None
    fsWriter.Move <- fun s d ->
        moved <- Some (s, d)
        Ok ()
    let model = createModel()
    if curPathDifferent then
        model.Path <- createPath "/c/other"
    let res = MainLogic.Action.undoRename_m fsReader fsWriter prevNode curNode.Name model

    res |> shouldEqual (Ok ())
    moved |> shouldEqual (Some (curNode.Path, prevNode.Path))
    let expected = createModel()
    if curPathDifferent then
        expected.BackStack <- (createPath "/c/other", expected.Cursor) :: expected.BackStack
        expected.ForwardStack <- []
    expected.Nodes <- newNodes
    expected.Cursor <- 1
    assertAreEqual expected model

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo rename to path with existing item returns error`` existingHidden =
    let prevNode = { newNodes.[1] with IsHidden = existingHidden }
    let curNode = oldNodes.[1]
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- fun p -> if p = prevNode.Path then Ok (Some prevNode) else Ok None
    let fsWriter = FakeFileSystemWriter()
    let model = createModel()
    let res = MainLogic.Action.undoRename_m fsReader fsWriter prevNode curNode.Name model

    res |> shouldEqual (Error (CannotUseNameAlreadyExists ("rename", Folder, prevNode.Name, existingHidden)))
    let expected = createModel()
    assertAreEqual expected model

[<Test>]
let ``Undo rename item handles move error by returning error``() =
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- fun _ -> Ok None
    let fsWriter = FakeFileSystemWriter()
    fsWriter.Move <- fun _ _ -> Error ex
    let prevNode = newNodes.[1]
    let curNode = oldNodes.[1]
    let model = createModel()
    let res = MainLogic.Action.undoRename_m fsReader fsWriter prevNode curNode.Name model

    let expectedAction = RenamedItem (curNode, prevNode.Name)
    res |> shouldEqual (Error (ItemActionError (expectedAction, model.PathFormat, ex)))
    let expected = createModel()
    assertAreEqual expected model

// start rename selection tests

let renameTextSelection cursorPosition fileName =
    let node = createNode ("/c/path/" + fileName)
    let model = createModel()
    model.Nodes <- List.append oldNodes [node]
    model.Cursor <- model.Nodes.Length - 1
    let inputMode = Input (Rename cursorPosition)
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- fun _ -> Ok None
    MainLogic.Action.startInput_m fsReader inputMode model
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
