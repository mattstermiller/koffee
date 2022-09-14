module Koffee.MainActionTests_CreateRename

open NUnit.Framework
open FsUnitTyped
open Koffee.Main

// create tests

[<Test>]
let ``Create calls file sys create and openPath and sets status``() =
    let fs = FakeFileSystem [
        file "another"
    ]
    let createItem = createFile "/c/file"
    let model = testModel

    let actual = seqResult (Action.create fs File createItem.Name) model

    let expectedAction = CreatedItem createItem
    let expectedItems = [
        createFile "/c/another"
        createItem
    ]
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            Cursor = 1
            UndoStack = expectedAction :: model.UndoStack
            RedoStack = []
            Status = Some <| MainStatus.actionComplete expectedAction model.PathFormat
        }
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        file "another"
        file "file"
    ]

[<TestCase(false)>]
[<TestCase(true)>]
let ``Create returns error when item already exists at path`` existingHidden =
    let fs = FakeFileSystem [
        file "another"
        fileWith (hide existingHidden) "file"
    ]
    let existing = fs.Item "/c/file"
    let expectedFs = fs.Items

    let actual = seqResult (Action.create fs Folder existing.Name) testModel

    let expectedItems = [
        createFile "/c/another"
        existing
    ]
    let expected =
        { testModel with
            Directory = expectedItems
            Items = expectedItems
            Cursor = 1
        }.WithError (CannotUseNameAlreadyExists ("create", Folder, existing.Name, existingHidden))
    assertAreEqual expected actual
    fs.Items |> shouldEqual expectedFs

[<Test>]
let ``Create handles error by returning error``() =
    let fs = FakeFileSystem [
        file "another"
    ]
    let createItem = createFile "/c/file"
    fs.AddExnPath false ex createItem.Path
    let model = testModel
    let expectedFs = fs.Items

    let actual = seqResult (Action.create fs File createItem.Name) model

    let expected = model.WithError (ItemActionError ((CreatedItem createItem), model.PathFormat, ex))
    assertAreEqual expected actual
    fs.Items |> shouldEqual expectedFs

// undo create tests

[<TestCase(false, false)>]
[<TestCase(true, false)>]
[<TestCase(false, true)>]
[<TestCase(true, true)>]
let ``Undo create empty item calls delete`` curPathDifferent isFolder =
    let fs = FakeFileSystem [
        file "another"
        if isFolder then
            folder "item" []
        else
            file "item"
    ]
    let createdItem = fs.Item "/c/item"
    let action = CreatedItem createdItem
    let location = if curPathDifferent then "/c/other" else "/c"
    let model =
        testModel.WithStatus (ErrorMessage "previous error")
        |> withLocation location |> pushUndo action

    let actual = seqResult (Action.undo fs testProgress) model

    let expectedItems =
        if curPathDifferent then
            model.Items
        else
            [ createFile "/c/another" ]
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            UndoStack = model.UndoStack.Tail
            RedoStack = action :: model.RedoStack
            Status = Some (MainStatus.undoAction action model.PathFormat 1)
        }
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        file "another"
    ]
    fs.RecycleBin |> shouldEqual []

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo create non empty item returns error`` isFolder =
    let fs = FakeFileSystem [
        if isFolder then
            folder "item" [
                file "file"
            ]
        else
            fileWith (size 1L) "item"
    ]
    let createdItem = fs.Item "/c/item"
    let action = CreatedItem createdItem
    let model = testModel |> pushUndo action
    let expectedFs = fs.Items

    let actual = seqResult (Action.undo fs testProgress) model

    let expected = model.WithError (CannotUndoNonEmptyCreated createdItem) |> popUndo
    assertAreEqual expected actual
    fs.Items |> shouldEqual expectedFs

[<Test>]
let ``Undo create handles delete error by returning error`` () =
    let fs = FakeFileSystem [
        file "file"
    ]
    let createdItem = fs.Item "/c/file"
    fs.AddExnPath false ex createdItem.Path
    let action = CreatedItem createdItem
    let model = testModel |> pushUndo action

    let actual = seqResult (Action.undo fs testProgress) model

    let expected = model.WithError (ItemActionError (DeletedItem (createdItem, true), model.PathFormat, ex)) |> popUndo
    assertAreEqual expected actual

[<Test>]
let ``Redo create creates item again`` () =
    let fs = FakeFileSystem [
        file "another"
    ]
    let createItem = createFile "/c/file"
    let model = testModel |> pushRedo (CreatedItem createItem)

    let actual = seqResult (Action.redo fs testProgress) model

    let expectedAction = CreatedItem createItem
    let expectedItems = [
        createFile "/c/another"
        createItem
    ]
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            Cursor = 1
            UndoStack = expectedAction :: model.UndoStack
            RedoStack = model.RedoStack.Tail
            Status = Some <| MainStatus.redoAction expectedAction model.PathFormat 1
        }
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        file "another"
        file "file"
    ]

[<Test>]
let ``Redo create handles error by returning error``() =
    let fs = FakeFileSystem [
        file "another"
    ]
    let createItem = createFile "/c/file"
    fs.AddExnPath false ex createItem.Path
    let model = testModel |> pushRedo (CreatedItem createItem)
    let expectedFs = fs.Items

    let actual = seqResult (Action.redo fs testProgress) model

    let expected =
        model.WithError (ItemActionError ((CreatedItem createItem), model.PathFormat, ex))
        |> popRedo
    assertAreEqual expected actual
    fs.Items |> shouldEqual expectedFs

// rename tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Rename calls file sys move and openPath and sets status`` diffCaseOnly =
    let fs = FakeFileSystem [
        file "another"
        file "my file"
        file "nacho file"
    ]
    let item = fs.Item "/c/my file"
    let renamed = createFile (if diffCaseOnly then "/c/My File" else "/c/renamed")
    let items = fs.ItemsIn "/c"
    let model = { testModel with Directory = items; Items = items; Cursor = 1 }

    let actual = Action.rename fs item renamed.Name model
                 |> assertOk

    let expectedItems = [
        createFile "/c/another"
        renamed
        createFile "/c/nacho file"
    ]
    let expectedAction = RenamedItem (item, renamed.Name)
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems |> sortByPath
            Cursor = if diffCaseOnly then 1 else 2
            UndoStack = expectedAction :: model.UndoStack
            RedoStack = []
            Status = Some <| MainStatus.actionComplete expectedAction model.PathFormat
        }
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        file "another"
        file renamed.Name
        file "nacho file"
    ]

[<Test>]
let ``Rename in search result calls file sys move and sets status`` () =
    let fs = FakeFileSystem [
        file "another"
        file "file"
        file "nacho file"
    ]
    let item = fs.Item "/c/file"
    let renamed = createFile "/c/renamed"
    let items = fs.ItemsIn "/c"
    let model =
        { testModel with
            Directory = items
            Items = items |> List.skip 1
            Cursor = 1
            SearchCurrent = Some { Search.Default with Terms = "file" }
        }

    let actual = Action.rename fs item renamed.Name model
                 |> assertOk

    let expectedItems = [
        createFile "/c/another"
        createFile "/c/renamed"
        createFile "/c/nacho file"
    ]
    let sort = List.sortBy (fun i -> i.Name)
    fs.ItemsIn "/c" |> sort |> shouldEqual (sort expectedItems)
    let expectedAction = RenamedItem (item, renamed.Name)
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems |> List.skip 1
            Cursor = 1
            UndoStack = expectedAction :: model.UndoStack
            RedoStack = []
            Status = Some <| MainStatus.actionComplete expectedAction model.PathFormat
        }
    assertAreEqual expected actual

[<TestCase(false)>]
[<TestCase(true)>]
let ``Rename to path with existing item returns error`` existingHidden =
    let newName = "renamed"
    let fs = FakeFileSystem [
        fileWith (hide existingHidden) newName
        file "file"
    ]
    let item = fs.Item "/c/file"
    let model = testModel
    let expectedFs = fs.Items

    let actual = Action.rename fs item newName model

    actual |> shouldEqual (Error (CannotUseNameAlreadyExists ("rename", File, newName, existingHidden)))
    fs.Items |> shouldEqual expectedFs

[<Test>]
let ``Rename handles error by returning error``() =
    let fs = FakeFileSystem [
        file "another"
        file "file"
    ]
    let item = fs.Item "/c/file"
    let newName = "renamed"
    fs.AddExn false ex ("/c/" + newName)
    let model = testModel

    let actual = Action.rename fs item newName model

    let expectedAction = RenamedItem (item, newName)
    actual |> shouldEqual (Error (ItemActionError (expectedAction, model.PathFormat, ex)))

// undo rename tests

[<TestCase(false, false)>]
[<TestCase(true, false)>]
[<TestCase(false, true)>]
[<TestCase(true, true)>]
let ``Undo rename item names item back to original`` curPathDifferent diffCaseOnly =
    let currentName = if diffCaseOnly then "File" else "renamed"
    let fs = FakeFileSystem [
        file "another"
        file currentName
    ]
    let previous = createFile "/c/file"
    let current = fs.Item ("/c/" + currentName)
    let location = if curPathDifferent then "/c/other" else "/c"
    let action = RenamedItem (previous, current.Name)
    let model = testModel |> withLocation location |> pushUndo action

    let actual = seqResult (Action.undo fs testProgress) model

    let expectedItems = [
        createFile "/c/another"
        previous
    ]
    let expected =
        { testModel with
            Directory = expectedItems
            Items = expectedItems
            Cursor = 1
            RedoStack = action :: model.RedoStack
            Status = Some (MainStatus.undoAction action model.PathFormat 1)
        } |> withBackIf curPathDifferent (model.Location, 0)
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        file "another"
        file previous.Name
    ]

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo rename to path with existing item returns error`` existingHidden =
    let fs = FakeFileSystem [
        file "another"
        fileWith (hide existingHidden) "file"
        file "renamed"
    ]
    let previous = createFile "/c/file"
    let current = fs.Item "/c/renamed"
    let action = RenamedItem (previous, current.Name)
    let model = testModel |> pushUndo action
    let expectedFs = fs.Items

    let actual = seqResult (Action.undo fs testProgress) model

    let expectedError = CannotUseNameAlreadyExists ("rename", File, previous.Name, existingHidden)
    let expected = model.WithError expectedError |> popUndo
    assertAreEqual expected actual
    fs.Items |> shouldEqual expectedFs

[<Test>]
let ``Undo rename item handles move error by returning error``() =
    let fs = FakeFileSystem [
        file "renamed"
    ]
    let previous = createFile "/c/file"
    let current = fs.Item "/c/renamed"
    fs.AddExnPath false ex previous.Path
    let action = RenamedItem (previous, current.Name)
    let model = testModel |> pushUndo action
    let expectedFs = fs.Items

    let actual = seqResult (Action.undo fs testProgress) model

    let expectedError = ItemActionError (RenamedItem (current, previous.Name), model.PathFormat, ex)
    let expected = model.WithError expectedError |> popUndo
    assertAreEqual expected actual
    fs.Items |> shouldEqual expectedFs

// start rename selection tests

let renameTextSelection inputPosition itemType fileName =
    let item = { createFile ("/c/" + fileName) with Type = itemType }
    let items = [ createFile "/c/another"; item ]
    let model =
        { testModel with
            Items = items
            Cursor = items.Length - 1
            InputTextSelection = (1, 1)
        }
    let inputMode = Input (Rename inputPosition)
    let fs = FakeFileSystem []

    let actual = Action.startInput fs inputMode model
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
