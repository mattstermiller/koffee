module Koffee.MainLogicTests_CreateRename

open NUnit.Framework
open FsUnitTyped
open KellermanSoftware.CompareNetObjects

let oldNodes = [
    createNode "/c/path/one"
    createNode "/c/path/two"
]

let newNodes = [
    createNode "/c/path/new one"
    createNode "/c/path/new two"
]

let testModel =
    { baseModel with
        Location = createPath "/c/path"
        Nodes = oldNodes
        Cursor = 0
        InputText = ""
        InputTextSelection = (1, 1)
    }

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

    let actual = seqResult (MainLogic.Action.create fsReader fsWriter Folder createNode.Name) testModel

    created |> shouldEqual (Some (createNode.Type, createNode.Path))
    let expectedAction = CreatedItem createNode
    let expected =
        { testModel with
            Nodes = newNodes
            Cursor = 1
            UndoStack = expectedAction :: testModel.UndoStack
            RedoStack = []
            Status = Some <| MainStatus.actionComplete expectedAction testModel.PathFormat
        }
    assertAreEqual expected actual

[<TestCase(false)>]
[<TestCase(true)>]
let ``Create folder returns error when item already exists at path`` existingHidden =
    let existing = { oldNodes.[1] with IsHidden = existingHidden }
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- fun _ -> Ok <| Some existing
    fsReader.GetNodes <- fun _ _ -> Ok newNodes
    let fsWriter = FakeFileSystemWriter()
    let createNode = newNodes.[1]

    let actual = seqResult (MainLogic.Action.create fsReader fsWriter Folder createNode.Name) testModel

    let expected =
        { testModel with
            Nodes = newNodes
            Cursor = 0
        }.WithError (CannotUseNameAlreadyExists ("create", Folder, createNode.Name, existingHidden))
    assertAreEqual expected actual

[<Test>]
let ``Create folder handles error by returning error``() =
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- fun _ -> Ok None
    let fsWriter = FakeFileSystemWriter()
    fsWriter.Create <- fun _ _ -> Error ex
    let createNode = newNodes.[1]

    let actual = seqResult (MainLogic.Action.create fsReader fsWriter Folder createNode.Name) testModel

    let expected = testModel.WithError (ItemActionError ((CreatedItem createNode), testModel.PathFormat, ex))
    assertAreEqual expected actual

// undo create tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo create empty node calls delete`` curPathDifferent =
    let fsReader = FakeFileSystemReader()
    fsReader.IsEmpty <- fun _ -> true
    fsReader.GetNodes <- fun _ _ -> Ok newNodes
    let fsWriter = FakeFileSystemWriter()
    let mutable deleted = None
    fsWriter.Delete <- fun p ->
        deleted <- Some p
        Ok ()
    let createdNode = oldNodes.[1]
    let location = if curPathDifferent then createPath "/c/other" else testModel.Location
    let model = { testModel with Location = location }
    let actual = seqResult (MainLogic.Action.undoCreate fsReader fsWriter createdNode) model

    deleted |> shouldEqual (Some createdNode.Path)
    let expected =
        if curPathDifferent then
            model
        else
            { model with Nodes = newNodes; Cursor = 0 }
    CompareLogic() |> ignoreMembers ["Status"] |> assertAreEqualWith expected actual

[<Test>]
let ``Undo create non-empty node returns error``() =
    let fsReader = FakeFileSystemReader()
    fsReader.IsEmpty <- fun _ -> false
    let fsWriter = FakeFileSystemWriter()
    let createdNode = oldNodes.[1]
    let actual = seqResult (MainLogic.Action.undoCreate fsReader fsWriter createdNode) testModel

    let expected = testModel.WithError (CannotUndoNonEmptyCreated createdNode)
    assertAreEqual expected actual

[<Test>]
let ``Undo create handles delete error by returning error`` () =
    let fsReader = FakeFileSystemReader()
    fsReader.IsEmpty <- fun _ -> true
    let fsWriter = FakeFileSystemWriter()
    fsWriter.Delete <- fun _ -> Error ex
    let createdNode = oldNodes.[1]
    let actual = seqResult (MainLogic.Action.undoCreate fsReader fsWriter createdNode) testModel

    let expected = testModel.WithError (ItemActionError (DeletedItem (createdNode, true), testModel.PathFormat, ex))
    CompareLogic() |> ignoreMembers ["Status"] |> assertAreEqualWith expected actual

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

    let actual = MainLogic.Action.rename fsReader fsWriter currentNode renamedNode.Name testModel
                 |> assertOk

    renamed |> shouldEqual (Some (currentNode.Path, renamedNode.Path))
    let expectedAction = RenamedItem (currentNode, renamedNode.Name)
    let expected =
        { testModel with
            Nodes = newNodes
            Cursor = if diffCaseOnly then 0 else 1
            UndoStack = expectedAction :: testModel.UndoStack
            RedoStack = []
            Status = Some <| MainStatus.actionComplete expectedAction testModel.PathFormat
        }
    assertAreEqual expected actual

[<TestCase(false)>]
[<TestCase(true)>]
let ``Rename to path with existing item returns error`` existingHidden =
    let currentNode = oldNodes.[1]
    let renamedNode = { newNodes.[1] with IsHidden = existingHidden }
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- fun p -> if p = renamedNode.Path then Ok (Some renamedNode) else Ok None
    let fsWriter = FakeFileSystemWriter()

    let actual = MainLogic.Action.rename fsReader fsWriter currentNode renamedNode.Name testModel

    actual |> shouldEqual (Error (CannotUseNameAlreadyExists ("rename", Folder, renamedNode.Name, existingHidden)))

[<Test>]
let ``Rename handles error by returning error``() =
    let currentNode = oldNodes.[1]
    let renamedNode = newNodes.[1]
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- fun _ -> Ok None
    let fsWriter = FakeFileSystemWriter()
    fsWriter.Move <- fun _ _ -> Error ex

    let actual = MainLogic.Action.rename fsReader fsWriter currentNode renamedNode.Name testModel

    let expectedAction = RenamedItem (currentNode, renamedNode.Name)
    actual |> shouldEqual (Error (ItemActionError (expectedAction, testModel.PathFormat, ex)))

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
    let location = if curPathDifferent then createPath "/c/other" else testModel.Location
    let model = { testModel with Location = location }

    let actual = MainLogic.Action.undoRename fsReader fsWriter prevNode curNode.Name model
                 |> assertOk

    moved |> shouldEqual (Some (curNode.Path, prevNode.Path))
    let expected = { testModel with Nodes = newNodes; Cursor = 1 }
    let expected =
        if curPathDifferent then
            { expected with
                BackStack = (location, 0) :: expected.BackStack
                ForwardStack = []
            }
        else expected
    assertAreEqual expected actual

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo rename to path with existing item returns error`` existingHidden =
    let prevNode = { newNodes.[1] with IsHidden = existingHidden }
    let curNode = oldNodes.[1]
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- fun p -> if p = prevNode.Path then Ok (Some prevNode) else Ok None
    let fsWriter = FakeFileSystemWriter()

    let actual = MainLogic.Action.undoRename fsReader fsWriter prevNode curNode.Name testModel

    actual |> shouldEqual (Error (CannotUseNameAlreadyExists ("rename", Folder, prevNode.Name, existingHidden)))

[<Test>]
let ``Undo rename item handles move error by returning error``() =
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- fun _ -> Ok None
    let fsWriter = FakeFileSystemWriter()
    fsWriter.Move <- fun _ _ -> Error ex
    let prevNode = newNodes.[1]
    let curNode = oldNodes.[1]

    let actual = MainLogic.Action.undoRename fsReader fsWriter prevNode curNode.Name testModel

    let expectedAction = RenamedItem (curNode, prevNode.Name)
    actual |> shouldEqual (Error (ItemActionError (expectedAction, testModel.PathFormat, ex)))

// start rename selection tests

let renameTextSelection cursorPosition fileName =
    let node = createNode ("/c/path/" + fileName)
    let nodes = List.append oldNodes [node]
    let model =
        { baseModel with
            Nodes = nodes
            Cursor = nodes.Length - 1
        }
    let inputMode = Input (Rename cursorPosition)
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- fun _ -> Ok None
    let actual = MainLogic.Action.startInput fsReader inputMode model
                 |> assertOk

    actual.InputMode |> shouldEqual (Some inputMode)
    actual.InputText |> shouldEqual node.Name
    actual.InputTextSelection

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
