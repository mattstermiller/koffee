module Koffee.MainActionTests_Delete

open NUnit.Framework
open FsUnitTyped
open Koffee.Main

let testModelFromFs (fs: FakeFileSystem)  =
    let items = fs.ItemsIn "/c"
    { testModel with
        Directory = items
        Items = items
    }

[<Test>]
let ``Recycle NetHost path removes it from items and history`` () =
    let fs = FakeFileSystem [
        network [
            netHost "host1" []
            netHost "host2" []
        ]
    ]
    let items = fs.ItemsIn "/net"
    let model =
        { testModel with
            Directory = items
            Items = items
            Cursor = 1
            History =
                { testModel.History with
                    NetHosts = ["host1"; "host2"]
                    Paths = [
                        createHistoryPath "/net/host1/share1"
                        createHistoryPath "/net/host2/share2"
                    ]
                }
        }

    let actual = seqResult (Action.recycle fs model.SelectedItem) model

    let expectedItems = items |> List.take 1
    let expected =
        { testModel with
            Directory = expectedItems
            Items = expectedItems
            Cursor = 0
            History =
                { testModel.History with
                    NetHosts = ["host1"]
                    // currently expecting path history to be preserved, but we might want to remove sub-paths in the future
                    Paths = model.History.Paths
                }
        }
        |> MainModel.withMessage (MainStatus.RemovedNetworkHost "host2")
    assertAreEqual expected actual
    assertAreEqual expected.History actual.History

[<TestCase(true)>]
[<TestCase(false)>]
let ``Recycle or Delete file recycles or deletes it and updates path history`` permanent =
    let fs = FakeFileSystem [
        driveWithSize 'c' 100L [
            fileWith (size 2L) "file"
            file "other"
        ]
    ]
    let item = fs.Item "/c/file"
    let model =
        testModelFromFs fs
        |> withHistoryPaths (historyPaths {
            "/c/folder2/unrelated"
            item
        })

    let testFunc = if permanent then Action.delete fs progress else Action.recycle fs
    let actual = seqResult (testFunc item) model

    let expectedItems = [createFile "/c/other"]
    let expectedAction = DeletedItem (item, permanent)
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            UndoStack = expectedAction :: model.UndoStack
            RedoStack = []
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (MainStatus.ActionComplete (expectedAction, model.PathFormat))
        |> withHistoryPaths (model.History.Paths |> List.take 1)
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        driveWithSize 'c' 100L [
            file "other"
        ]
    ]
    fs.RecycleBin |> shouldEqual (if permanent then [] else [item])

[<TestCase(false)>]
[<TestCase(true)>]
let ``Recycle or Delete folder recycles or deletes it and updates path history`` permanent =
    let fs = FakeFileSystem [
        driveWithSize 'c' 100L [
            folder "folder" [
                folder "sub" [
                    folder "empty" []
                    file "sub file"
                ]
                file "file"
            ]
            file "other"
        ]
    ]
    let item = fs.Item "/c/folder"
    let model =
        testModelFromFs fs
        |> withHistoryPaths (historyPaths {
            "/c/folder2/unrelated"
            item
            "/c/folder/sub/sub file"
        })

    let testFunc = if permanent then Action.delete fs progress else Action.recycle fs
    let actual = seqResult (testFunc item) model

    let expectedItems = [createFile "/c/other"]
    let expectedAction = DeletedItem (item, permanent)
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            UndoStack = expectedAction :: model.UndoStack
            RedoStack = []
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (MainStatus.ActionComplete (expectedAction, model.PathFormat))
        |> withHistoryPaths (model.History.Paths |> List.take 1)
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        driveWithSize 'c' 100L [
            file "other"
        ]
    ]
    fs.RecycleBin |> shouldEqual (if permanent then [] else [item])

// TODO: When implementing multi-select, extend or write similar test to cover canceling a recycle operation
[<TestCase(false)>]
[<TestCase(true)>]
let ``Delete or Redo delete folder deletes items until canceled, then Delete or Redo again resumes and merges undo item``
        isRedo =
    let fs = FakeFileSystem [
        folder "folder" [
            folder "sub" [
                folder "empty" []
                file "sub file"
            ]
            file "z last"
        ]
        file "other"
    ]
    let item = fs.Item "/c/folder"
    let action = DeletedItem (item, true)
    let model =
        testModelFromFs fs
        |> if isRedo then pushRedo action else id

    let testFunc =
        if isRedo
        then Action.redo fs progress
        else Action.delete fs progress item

    // part one: delete cancels correctly
    let modelAfterCancel = seqResultWithCancelTokenCallback (fs.CancelAfterWriteCount 2) testFunc model

    (
        let expected =
            { model with
                UndoStack = action :: testModel.UndoStack
                RedoStack = action :: if isRedo then testModel.RedoStack else []
            }
            |> MainModel.withMessage (MainStatus.CancelledDelete (true, 2, 5))
        assertAreEqual expected modelAfterCancel
        fs.ItemsShouldEqual [
            folder "folder" [
                folder "sub" []
                file "z last"
            ]
            file "other"
        ]
        fs.RecycleBin |> shouldEqual []
        printfn "part one: cancellation completed successfully"
    )

    // part two: delete or redo again completes the operation and merges undo item
    let actual = modelAfterCancel |> seqResult testFunc

    let expectedItems = [
        createFile "/c/other"
    ]
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            UndoStack = action :: testModel.UndoStack
            RedoStack = if isRedo then testModel.RedoStack else []
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (
            if isRedo
            then MainStatus.RedoAction (action, model.PathFormat, 1, 1)
            else MainStatus.ActionComplete (action, model.PathFormat)
        )
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        file "other"
    ]
    fs.RecycleBin |> shouldEqual []

[<TestCase(false)>]
[<TestCase(true)>]
let ``Recycle or Delete folder does nothing when canceled immediately`` permanent =
    let fs = FakeFileSystem [
        driveWithSize 'c' 100L [
            folder "folder" [
                folder "sub" [
                    folder "empty" []
                    file "sub file"
                ]
                file "z last"
            ]
            file "other"
        ]
    ]
    let item = fs.Item "/c/folder"
    let model = testModelFromFs fs
    let expectedFs = fs.Items

    let testFunc = if permanent then Action.delete fs progress else Action.recycle fs
    let actual = seqResultWithCancelTokenCallback (fun ct -> ct.Cancel()) (testFunc item) model

    let expectedTotal = if permanent then 0 else 1
    let expected = model |> MainModel.withMessage (MainStatus.CancelledDelete (permanent, 0, expectedTotal))
    assertAreEqual expected actual
    fs.ItemsShouldEqualList expectedFs

[<TestCase(false)>]
[<TestCase(true)>]
let ``Recycle file or folder that does not fit in the Recycle Bin returns error`` isFolder =
    let fs = FakeFileSystem [
        driveWithSize 'c' 100L [
            folder "folder" [
                folder "sub" [
                    fileWith (size 1L) "file 1"
                    folder "sub sub" [
                        fileWith (size 1L) "file 2"
                    ]
                ]
                fileWith (size 1L) "file 3"
                fileWith (size 1L) "file 4"
            ]
            fileWith (size 4L) "big file"
        ]
    ]
    let item =
        if isFolder
        then fs.Item "/c/folder"
        else fs.Item "/c/big file"
    let model = testModelFromFs fs
    let expectedFs = fs.Items

    let actual = seqResult (Action.recycle fs item) model

    let expectedEx = FakeFileSystemErrors.cannotRecycleItemThatDoesNotFit 4L
    let expectedError = MainStatus.ItemActionError (DeletedItem (item, false), model.PathFormat, expectedEx)
    let expected = model |> MainModel.withError expectedError |> withNewCancelToken
    assertAreEqual expected actual
    fs.Items |> shouldEqual expectedFs

[<Test>]
let ``Recycle folder that contains folder that cannot be read returns error`` () =
    let fs = FakeFileSystem [
        driveWithSize 'c' 100L [
            folder "folder" [
                folder "sub" [
                    fileWith (size 1L) "file 1"
                ]
                fileWith (size 1L) "file 2"
                fileWith (size 1L) "file 3"
            ]
            file "file"
        ]
    ]
    fs.AddExn false ex "/c/folder/sub"
    let item = fs.Item "/c/folder"
    let model = testModelFromFs fs
    let expectedFs = fs.Items

    let actual = seqResult (Action.recycle fs item) model

    let expectedError = MainStatus.ActionError ("check folder content size", ex)
    let expected = model |> MainModel.withError expectedError |> withNewCancelToken
    assertAreEqual expected actual
    fs.Items |> shouldEqual expectedFs

// TODO: When implementing multi-select, write similar test to cover recycle operation with multiple items
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
    let model = testModelFromFs fs

    let actual = seqResult (Action.delete fs progress item) model

    let expectedErrorItems = [
        createFile "/c/folder/sub/sub file", ex
        createFolder "/c/folder/sub", FakeFileSystemErrors.cannotDeleteNonEmptyFolder
        createFolder "/c/folder", FakeFileSystemErrors.cannotDeleteNonEmptyFolder
    ]
    let expected = model |> MainModel.withError (MainStatus.DeleteError (expectedErrorItems, 5)) |> withNewCancelToken
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        folder "folder" [
            folder "sub" [
                file "sub file"
            ]
        ]
        file "other"
    ]
    fs.RecycleBin |> shouldEqual []

[<TestCase(false)>]
[<TestCase(true)>]
let ``Recycle or Delete item handles error by returning error`` permanent =
    let fs = FakeFileSystem [
        file "file"
        file "other"
    ]
    let item = fs.Item "/c/file"
    fs.AddExnPath false ex item.Path
    let model = testModelFromFs fs
    let expectedFs = fs.Items

    let testFunc = if permanent then Action.delete fs progress else Action.recycle fs
    let actual = seqResult (testFunc item) model

    let expectedError =
        if permanent then
            MainStatus.DeleteError ([item, ex], 1)
        else
            MainStatus.ItemActionError (DeletedItem (item, false), model.PathFormat, ex)
    let expected = model |> MainModel.withError expectedError |> withNewCancelToken
    assertAreEqual expected actual
    fs.Items |> shouldEqual expectedFs
