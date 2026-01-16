module Koffee.ItemCreateTests

open NUnit.Framework
open FsUnitTyped

// create tests

[<Test>]
let ``Create calls file sys create and openPath and sets status``() =
    let fs = FakeFileSystem [
        file "another"
    ]
    let createItem = createFile "/c/file"
    let model = testModel

    let actual = seqResult (ItemActionCommands.Create.create fs File createItem.Name) model

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
        }
        |> MainModel.withMessage (MainStatus.ActionComplete expectedAction)
        |> withLocationOnHistory
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

    let actual = seqResult (ItemActionCommands.Create.create fs Folder existing.Name) testModel

    let expectedItems = [
        createFile "/c/another"
        existing
    ]
    let expected =
        { testModel with
            Directory = expectedItems
            Items = expectedItems
            Cursor = 1
        }
        |> MainModel.withError (MainStatus.CannotUseNameAlreadyExists ("create", Folder, existing.Name, existingHidden))
        |> withLocationOnHistory
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

    let actual = seqResult (ItemActionCommands.Create.create fs File createItem.Name) model

    let expected = model |> MainModel.withError (MainStatus.ItemActionError ((CreatedItem createItem), ex))
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
        testModel
        |> MainModel.withError MainStatus.NoPreviousSearch
        |> withLocation location
        |> pushUndo action
        |> withHistoryPaths (historyPaths {
            createdItem
            if isFolder then
                createdItem.Path.Join "file", false
        })

    let actual = seqResult (ItemActionCommands.Undo.undo fs progress) model

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
            MainModel.History.Paths = []
        }
        |> MainModel.withMessage (MainStatus.UndoAction (action, 1, 1))
        |> applyIf (not curPathDifferent) withLocationOnHistory
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

    let actual = seqResult (ItemActionCommands.Undo.undo fs progress) model

    let expected = model |> MainModel.withError (MainStatus.CannotUndoNonEmptyCreated createdItem) |> popUndo
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

    let actual = seqResult (ItemActionCommands.Undo.undo fs progress) model

    let expectedError = MainStatus.ItemActionError (DeletedItems (true, [createdItem], false), ex)
    let expected = model |> MainModel.withError expectedError |> popUndo
    assertAreEqual expected actual

[<Test>]
let ``Redo create creates item again`` () =
    let fs = FakeFileSystem [
        file "another"
    ]
    let createItem = createFile "/c/file"
    let model = testModel |> pushRedo (CreatedItem createItem)

    let actual = seqResult (ItemActionCommands.Undo.redo fs progress) model

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
        }
        |> MainModel.withMessage (MainStatus.RedoAction (expectedAction, 1, 1))
        |> withLocationOnHistory
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

    let actual = seqResult (ItemActionCommands.Undo.redo fs progress) model

    let expected =
        model
        |> MainModel.withError (MainStatus.ItemActionError (CreatedItem createItem, ex))
        |> popRedo
    assertAreEqual expected actual
    fs.Items |> shouldEqual expectedFs
