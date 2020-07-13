module Koffee.MainLogicTests_CreateRename

open NUnit.Framework
open FsUnitTyped

let oldItems = [
    createItem "/c/path/one"
    createItem "/c/path/two"
]

let newItems = [
    createItem "/c/path/new one"
    createItem "/c/path/new two"
]

let testModel =
    { baseModel.WithLocation (createPath "/c/path") with
        Directory = oldItems
        Items = oldItems
        Cursor = 0
        InputText = ""
        InputTextSelection = (1, 1)
    }

let ex = System.UnauthorizedAccessException() :> exn

// create tests

[<Test>]
let ``Create folder calls file sys create, openPath and sets status``() =
    let fsReader = FakeFileSystemReader()
    fsReader.GetItem <- fun _ -> Ok None
    fsReader.GetItems <- fun _ -> Ok newItems
    let fsWriter = FakeFileSystemWriter()
    let mutable created = None
    fsWriter.Create <- fun itemType path ->
        created <- Some (itemType, path)
        Ok ()
    let createItem = newItems.[1]

    let actual = seqResult (MainLogic.Action.create (FileSystemComp(fsReader, fsWriter)) Folder createItem.Name) testModel

    created |> shouldEqual (Some (createItem.Type, createItem.Path))
    let expectedAction = CreatedItem createItem
    let expected =
        { testModel with
            Directory = newItems
            Items = newItems
            Cursor = 1
            UndoStack = expectedAction :: testModel.UndoStack
            RedoStack = []
            Status = Some <| MainStatus.actionComplete expectedAction testModel.PathFormat
        }
    assertAreEqual expected actual

[<TestCase(false)>]
[<TestCase(true)>]
let ``Create folder returns error when item already exists at path`` existingHidden =
    let existing = { oldItems.[1] with IsHidden = existingHidden }
    let fsReader = FakeFileSystemReader()
    fsReader.GetItem <- fun _ -> Ok <| Some existing
    fsReader.GetItems <- fun _ -> Ok newItems
    let fsWriter = FakeFileSystemWriter()
    let createItem = newItems.[1]

    let actual = seqResult (MainLogic.Action.create (FileSystemComp(fsReader, fsWriter)) Folder createItem.Name) testModel

    let expected =
        { testModel with
            Directory = newItems
            Items = newItems
            Cursor = 0
        }.WithError (CannotUseNameAlreadyExists ("create", Folder, createItem.Name, existingHidden))
    assertAreEqual expected actual

[<Test>]
let ``Create folder handles error by returning error``() =
    let fsReader = FakeFileSystemReader()
    fsReader.GetItem <- fun _ -> Ok None
    let fsWriter = FakeFileSystemWriter()
    fsWriter.Create <- fun _ _ -> Error ex
    let createItem = newItems.[1]

    let actual = seqResult (MainLogic.Action.create (FileSystemComp(fsReader, fsWriter)) Folder createItem.Name) testModel

    let expected = testModel.WithError (ItemActionError ((CreatedItem createItem), testModel.PathFormat, ex))
    assertAreEqual expected actual

// undo create tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo create empty item calls delete`` curPathDifferent =
    let fsReader = FakeFileSystemReader()
    fsReader.IsEmpty <- fun _ -> true
    fsReader.GetItems <- fun _ -> Ok newItems
    let fsWriter = FakeFileSystemWriter()
    let mutable deleted = None
    fsWriter.Delete <- fun p ->
        deleted <- Some p
        Ok ()
    let createdItem = oldItems.[1]
    let location = if curPathDifferent then createPath "/c/other" else testModel.Location
    let model = testModel.WithLocation location

    let actual = seqResult (MainLogic.Action.undoCreate (FileSystemComp(fsReader, fsWriter)) createdItem) model

    deleted |> shouldEqual (Some createdItem.Path)
    let expected =
        if curPathDifferent then
            model
        else
            { model with
                Directory = newItems
                Items = newItems
                Cursor = 0
            }
    assertAreEqualWith expected actual (ignoreMembers ["Status"])

[<Test>]
let ``Undo create non-empty item returns error``() =
    let fsReader = FakeFileSystemReader()
    fsReader.IsEmpty <- fun _ -> false
    let fsWriter = FakeFileSystemWriter()
    let createdItem = oldItems.[1]

    let actual = seqResult (MainLogic.Action.undoCreate (FileSystemComp(fsReader, fsWriter)) createdItem) testModel

    let expected = testModel.WithError (CannotUndoNonEmptyCreated createdItem)
    assertAreEqual expected actual

[<Test>]
let ``Undo create handles delete error by returning error`` () =
    let fsReader = FakeFileSystemReader()
    fsReader.IsEmpty <- fun _ -> true
    let fsWriter = FakeFileSystemWriter()
    fsWriter.Delete <- fun _ -> Error ex
    let createdItem = oldItems.[1]

    let actual = seqResult (MainLogic.Action.undoCreate (FileSystemComp(fsReader, fsWriter)) createdItem) testModel

    let expected = testModel.WithError (ItemActionError (DeletedItem (createdItem, true), testModel.PathFormat, ex))
    assertAreEqual expected actual

// rename tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Rename calls file sys move, openPath and sets status`` diffCaseOnly =
    let currentItem = oldItems.[1]
    let renamedItem = if diffCaseOnly then currentItem else newItems.[1]
    let newItems = [oldItems.[0]; renamedItem]
    let fsReader = FakeFileSystemReader()
    fsReader.GetItem <- fun _ -> if diffCaseOnly then Ok (Some currentItem) else Ok None
    fsReader.GetItems <- fun _ -> Ok newItems
    let fsWriter = FakeFileSystemWriter()
    let mutable renamed = None
    fsWriter.Move <- fun s d ->
        renamed <- Some (s, d)
        Ok ()

    let actual = MainLogic.Action.rename (FileSystemComp(fsReader, fsWriter)) currentItem renamedItem.Name testModel
                 |> assertOk

    renamed |> shouldEqual (Some (currentItem.Path, renamedItem.Path))
    let expectedAction = RenamedItem (currentItem, renamedItem.Name)
    let expected =
        { testModel with
            Directory = newItems
            Items = newItems
            UndoStack = expectedAction :: testModel.UndoStack
            RedoStack = []
            Status = Some <| MainStatus.actionComplete expectedAction testModel.PathFormat
        }
    assertAreEqual expected actual

[<TestCase(false)>]
[<TestCase(true)>]
let ``Rename to path with existing item returns error`` existingHidden =
    let currentItem = oldItems.[1]
    let renamedItem = { newItems.[1] with IsHidden = existingHidden }
    let fsReader = FakeFileSystemReader()
    fsReader.GetItem <- fun p -> if p = renamedItem.Path then Ok (Some renamedItem) else Ok None
    let fsWriter = FakeFileSystemWriter()

    let actual = MainLogic.Action.rename (FileSystemComp(fsReader, fsWriter)) currentItem renamedItem.Name testModel

    actual |> shouldEqual (Error (CannotUseNameAlreadyExists ("rename", Folder, renamedItem.Name, existingHidden)))

[<Test>]
let ``Rename handles error by returning error``() =
    let currentItem = oldItems.[1]
    let renamedItem = newItems.[1]
    let fsReader = FakeFileSystemReader()
    fsReader.GetItem <- fun _ -> Ok None
    let fsWriter = FakeFileSystemWriter()
    fsWriter.Move <- fun _ _ -> Error ex

    let actual = MainLogic.Action.rename (FileSystemComp(fsReader, fsWriter)) currentItem renamedItem.Name testModel

    let expectedAction = RenamedItem (currentItem, renamedItem.Name)
    actual |> shouldEqual (Error (ItemActionError (expectedAction, testModel.PathFormat, ex)))

// undo rename tests

[<TestCase(false, false)>]
[<TestCase(true, false)>]
[<TestCase(false, true)>]
[<TestCase(true, true)>]
let ``Undo rename item names file back to original`` curPathDifferent diffCaseOnly =
    let prevItem = newItems.[1]
    let curItem = if diffCaseOnly then prevItem else oldItems.[1]
    let fsReader = FakeFileSystemReader()
    fsReader.GetItem <- fun _ -> if diffCaseOnly then Ok (Some curItem) else Ok None
    fsReader.GetItems <- fun _ -> Ok newItems
    let fsWriter = FakeFileSystemWriter()
    let mutable moved = None
    fsWriter.Move <- fun s d ->
        moved <- Some (s, d)
        Ok ()
    let location = if curPathDifferent then createPath "/c/other" else testModel.Location
    let model = testModel.WithLocation location

    let actual = MainLogic.Action.undoRename (FileSystemComp(fsReader, fsWriter)) prevItem curItem.Name model
                 |> assertOk

    moved |> shouldEqual (Some (curItem.Path, prevItem.Path))
    let expected =
        { testModel with
            Directory = newItems
            Items = newItems
            Cursor = 1
        } |> (fun m ->
            if curPathDifferent then
                { m with
                    BackStack = (location, 0) :: m.BackStack
                    ForwardStack = []
                }
            else m
        )
    assertAreEqual expected actual

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo rename to path with existing item returns error`` existingHidden =
    let prevItem = { newItems.[1] with IsHidden = existingHidden }
    let curItem = oldItems.[1]
    let fsReader = FakeFileSystemReader()
    fsReader.GetItem <- fun p -> if p = prevItem.Path then Ok (Some prevItem) else Ok None
    let fsWriter = FakeFileSystemWriter()

    let actual = MainLogic.Action.undoRename (FileSystemComp(fsReader, fsWriter)) prevItem curItem.Name testModel

    actual |> shouldEqual (Error (CannotUseNameAlreadyExists ("rename", Folder, prevItem.Name, existingHidden)))

[<Test>]
let ``Undo rename item handles move error by returning error``() =
    let fsReader = FakeFileSystemReader()
    fsReader.GetItem <- fun _ -> Ok None
    let fsWriter = FakeFileSystemWriter()
    fsWriter.Move <- fun _ _ -> Error ex
    let prevItem = newItems.[1]
    let curItem = oldItems.[1]

    let actual = MainLogic.Action.undoRename (FileSystemComp(fsReader, fsWriter)) prevItem curItem.Name testModel

    let expectedAction = RenamedItem (curItem, prevItem.Name)
    actual |> shouldEqual (Error (ItemActionError (expectedAction, testModel.PathFormat, ex)))

// start rename selection tests

let renameTextSelection cursorPosition itemType fileName =
    let item = { createItem ("/c/path/" + fileName) with Type = itemType }
    let items = List.append oldItems [item]
    let model =
        { baseModel with
            Items = items
            Cursor = items.Length - 1
        }
    let inputMode = Input (Rename cursorPosition)
    let fsReader = FakeFileSystemReader()
    fsReader.GetItem <- fun _ -> Ok None
    let actual = MainLogic.Action.startInput fsReader inputMode model
                 |> assertOk

    actual.InputMode |> shouldEqual (Some inputMode)
    actual.InputText |> shouldEqual item.Name
    actual.InputTextSelection

[<Test>]
let ``StartInput for rename at beginning sets InputText and selection``() =
    renameTextSelection Begin File "three.txt.old" |> shouldEqual (0, 0)

[<Test>]
let ``StartInput for rename at end of name sets InputText and selection``() =
    renameTextSelection EndName File "three.txt.old" |> shouldEqual (9, 0)
    renameTextSelection EndName Folder "three.txt.old" |> shouldEqual (13, 0)

[<Test>]
let ``StartInput for rename at end of full name sets InputText and selection``() =
    renameTextSelection End File "three.txt.old" |> shouldEqual (13, 0)
    renameTextSelection End Folder "three.txt.old" |> shouldEqual (13, 0)

[<Test>]
let ``StartInput for rename replace name sets InputText and selection``() =
    renameTextSelection ReplaceName File "three.txt.old" |> shouldEqual (0, 9)
    renameTextSelection ReplaceName Folder "three.txt.old" |> shouldEqual (0, 13)

[<Test>]
let ``StartInput for rename replace name with no name sets InputText and selection``() =
    renameTextSelection ReplaceName File ".txt" |> shouldEqual (0, 0)
    renameTextSelection ReplaceName Folder ".txt" |> shouldEqual (0, 4)

[<Test>]
let ``StartInput for rename replace all sets InputText and selection``() =
    renameTextSelection ReplaceAll File "three.txt.old" |> shouldEqual (0, 13)
    renameTextSelection ReplaceAll Folder "three.txt.old" |> shouldEqual (0, 13)

[<Test>]
let ``StartInput for rename replace all with no extension sets InputText and selection``() =
    renameTextSelection ReplaceAll File "three" |> shouldEqual (0, 5)

[<Test>]
let ``StartInput for rename replace all with just dot sets InputText and selection``() =
    renameTextSelection ReplaceAll File "three." |> shouldEqual (0, 6)
    renameTextSelection ReplaceAll Folder "three." |> shouldEqual (0, 6)
