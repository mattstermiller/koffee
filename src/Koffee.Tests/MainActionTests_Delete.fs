module Koffee.MainActionTests_Delete

open NUnit.Framework
open FsUnitTyped
open Koffee.Main

let testModel (fs: FakeFileSystem)  =
    let items = fs.ItemsIn "/c"
    { testModel with
        Directory = items
        Items = items
    }

[<TestCase(true)>]
[<TestCase(false)>]
let ``Delete file deletes it and updates path history`` permanent =
    let fs = FakeFileSystem [
        file "file"
        file "other"
    ]
    let item = fs.Item "/c/file"
    let model = testModel fs |> withPathHistory ["/c/folder2/unrelated"; "/c/file"]

    let actual = seqResult (Action.delete fs progress item permanent) model

    let expectedItems = [createFile "/c/other"]
    let expectedAction = DeletedItem (item, permanent)
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            UndoStack = expectedAction :: model.UndoStack
            RedoStack = []
        }.WithMessage (MainStatus.ActionComplete (expectedAction, model.PathFormat))
    assertAreEqual expected actual
    fs.ItemsShouldEqual [file "other"]
    fs.RecycleBin |> shouldEqual (if permanent then [] else [item])
    actual |> assertPathHistoryEqual (model.History.Paths |> List.take 1)

[<Test>]
let ``Recycle folder recycles it and updates path history`` () =
    let fs = FakeFileSystem [
        folder "folder" [
            folder "sub" [
                file "sub file"
            ]
            file "file"
        ]
        file "other"
    ]
    let item = fs.Item "/c/folder"
    let pathHistory = [
        "/c/folder2/unrelated"
        "/c/folder"
        "/c/folder/sub/sub file"
    ]
    let model = testModel fs |> withPathHistory pathHistory

    let actual = seqResult (Action.delete fs progress item false) model

    let expectedItems = [createFile "/c/other"]
    let expectedAction = DeletedItem (item, false)
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            UndoStack = expectedAction :: model.UndoStack
            RedoStack = []
        }.WithMessage (MainStatus.ActionComplete (expectedAction, model.PathFormat))
    assertAreEqual expected actual
    fs.ItemsShouldEqual [file "other"]
    fs.RecycleBin |> shouldEqual [item]
    actual |> assertPathHistoryEqual (model.History.Paths |> List.take 1)

[<Test>]
let ``Delete folder deletes all items and updates path history`` () =
    let fs = FakeFileSystem [
        folder "folder" [
            folder "sub" [
                folder "empty" []
                file "sub file"
            ]
            file "file"
        ]
        file "other"
    ]
    let pathHistory = [
        "/c/folder2/unrelated"
        "/c/folder"
        "/c/folder/sub/sub file"
    ]
    let item = fs.Item "/c/folder"
    let model = testModel fs |> withPathHistory pathHistory

    let actual = seqResult (Action.delete fs progress item true) model

    let expectedItems = [createFile "/c/other"]
    let expectedAction = DeletedItem (item, true)
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            UndoStack = expectedAction :: model.UndoStack
            RedoStack = []
        }.WithMessage (MainStatus.ActionComplete (expectedAction, model.PathFormat))
    assertAreEqual expected actual
    fs.ItemsShouldEqual [file "other"]
    fs.RecycleBin |> shouldEqual []
    actual |> assertPathHistoryEqual (model.History.Paths |> List.take 1)

[<Test>]
let ``Delete folder handles individual error and deletes other items and returns error`` () =
    let fs = FakeFileSystem [
        folder "folder" [
            folder "sub" [
                folder "empty" []
                file "sub file"
            ]
            file "file"
        ]
        file "other"
    ]
    fs.AddExn true ex "/c/folder/sub/sub file"
    let item = fs.Item "/c/folder"
    let model = testModel fs

    let actual = seqResult (Action.delete fs progress item true) model

    let expectedErrorItems = [
        createFile "/c/folder/sub/sub file", ex
        createFolder "/c/folder/sub", FakeFileSystemErrors.cannotDeleteNonEmptyFolder
        createFolder "/c/folder", FakeFileSystemErrors.cannotDeleteNonEmptyFolder
    ]
    let expected = model.WithError (MainStatus.DeleteError (expectedErrorItems, 5))
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        folder "folder" [
            folder "sub" [
                file "sub file"
            ]
        ]
        file "other"
    ]

[<TestCase(false)>]
[<TestCase(true)>]
let ``Recycle and Delete handle errors by returning error`` permanent =
    let fs = FakeFileSystem [
        file "file"
        file "other"
    ]
    let item = fs.Item "/c/file"
    fs.AddExnPath false ex item.Path
    let model = testModel fs
    let expectedFs = fs.Items

    let actual = seqResult (Action.delete fs progress item permanent) model

    let expectedError =
        if permanent then
            MainStatus.DeleteError ([item, ex], 1)
        else
            MainStatus.ItemActionError (DeletedItem (item, false), model.PathFormat, ex)
    let expected = model.WithError expectedError
    assertAreEqual expected actual
    fs.Items |> shouldEqual expectedFs
