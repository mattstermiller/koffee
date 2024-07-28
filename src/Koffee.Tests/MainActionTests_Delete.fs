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

    let actual = seqResult (Action.recycle fs progress model.ActionItems) model

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
        |> MainModel.withMessage (MainStatus.RemovedNetworkHosts ["host2"])
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

    let testFunc = if permanent then Action.delete else Action.recycle
    let actual = seqResult (testFunc fs progress [item]) model

    let expectedItems = [createFile "/c/other"]
    let expectedAction = DeletedItems (permanent, [item], false)
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            UndoStack = expectedAction :: model.UndoStack
            RedoStack = []
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (MainStatus.ActionComplete expectedAction)
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
let ``Recycle or Delete multiple items from recursive search recycles or deletes and updates items and path history``
        permanent =
    let fs = FakeFileSystem [
        driveWithSize 'c' 100L [
            folder "folder1" [
                file "another"
                file "file1"
            ]
            folder "folder2" [
                folder "sub" [
                    file "file2"
                ]
            ]
            file "file3"
            file "file4"
            file "other"
        ]
    ]
    let items =
        [
            "/c/folder1/file1"
            "/c/folder2/sub/file2"
            "/c/file3"
            "/c/file4"
        ]
        |> List.map fs.Item
    let selected = items |> List.take 3
    let model =
        { testModel with
            Directory = fs.ItemsIn "/c"
            Items = items
            SelectedItems = selected
            SearchCurrent = Some { Terms = "file"; SubFolders = true; CaseSensitive = false; Regex = false }
        }
        |> withHistoryPaths (historyPaths {
            "/c/folder1/"
            yield! items
        })

    let testFunc = if permanent then Action.delete else Action.recycle
    let actual = seqResult (testFunc fs progress selected) model

    let expectedAction = DeletedItems (permanent, selected, false)
    let expected =
        { model with
            Directory = [
                createFolder "/c/folder1"
                createFolder "/c/folder2"
                createFile "/c/file4"
                createFile "/c/other"
            ]
            Items = [createFile "/c/file4"]
            SelectedItems = []
            UndoStack = expectedAction :: model.UndoStack
            RedoStack = []
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (MainStatus.ActionComplete expectedAction)
        |> withHistoryPaths (historyPaths {
            "/c/folder1/"
            "/c/file4"
        })
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        driveWithSize 'c' 100L [
            folder "folder1" [
                file "another"
            ]
            folder "folder2" [
                folder "sub" []
            ]
            file "file4"
            file "other"
        ]
    ]
    fs.RecycleBin |> shouldEqual (if permanent then [] else selected)

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

    let testFunc = if permanent then Action.delete else Action.recycle
    let actual = seqResult (testFunc fs progress [item]) model

    let expectedItems = [createFile "/c/other"]
    let expectedAction = DeletedItems (permanent, [item], false)
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            UndoStack = expectedAction :: model.UndoStack
            RedoStack = []
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (MainStatus.ActionComplete expectedAction)
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
let ``Recycle or Redo recycle multiple items recycles until canceled, then Recycle or Redo again resumes and merges undo item``
        isRedo =
    let fs = FakeFileSystem [
        driveWithSize 'c' 100L [
            folder "folder" [
                file "sub file"
            ]
            file "file1"
            file "file2"
            file "other"
        ]
    ]
    let items =
        [
            "/c/folder"
            "/c/file1"
            "/c/file2"
        ] |> List.map fs.Item
    let action = DeletedItems (false, items, false)
    let otherItem = fs.Item "/c/other"
    let model =
        testModelFromFs fs
        |> fun model -> { model with SelectedItems = items }
        |> if isRedo then pushRedo action else id

    let testFunc =
        if isRedo
        then Action.redo fs progress
        else fun m -> Action.recycle fs progress m.SelectedItems m

    // part one: recycle cancels correctly
    let modelAfterCancel = seqResultWithCancelTokenCallback (fs.CancelAfterWriteCount 2) testFunc model

    let expectedCancelledItems = items |> List.skip 2

    (
        let expectedItems = expectedCancelledItems @ [otherItem]
        let expectedUndoAction = DeletedItems (false, items |> List.take 2, true)
        let expectedRedoAction = DeletedItems (false, expectedCancelledItems, true)
        let expected =
            { model with
                Directory = expectedItems
                Items = expectedItems
                SelectedItems = expectedCancelledItems
                UndoStack = expectedUndoAction :: testModel.UndoStack
                RedoStack = expectedRedoAction :: if isRedo then testModel.RedoStack else []
            }
            |> MainModel.withMessage (MainStatus.CancelledDelete (false, 2, items.Length))
        assertAreEqual expected modelAfterCancel
        fs.ItemsShouldEqual [
            driveWithSize 'c' 100L [
                file "file2"
                file "other"
            ]
        ]
        fs.RecycleBin |> shouldEqual [
            createFolder "/c/folder"
            createFile "/c/file1"
        ]
        printfn "part one: cancellation completed successfully"
    )

    // part two: delete or redo again completes the operation and merges undo item
    let actual = modelAfterCancel |> seqResult testFunc

    let expectedItems = [otherItem]
    let expectedAction = DeletedItems (false, expectedCancelledItems, false)
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            SelectedItems = []
            UndoStack = action :: testModel.UndoStack
            RedoStack = if isRedo then testModel.RedoStack else []
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (
            if isRedo
            then MainStatus.RedoAction (expectedAction, 1, 1)
            else MainStatus.ActionComplete expectedAction
        )
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        driveWithSize 'c' 100L [
            file "other"
        ]
    ]
    fs.RecycleBin |> shouldEqual [
        createFolder "/c/folder"
        createFile "/c/file1"
        createFile "/c/file2"
    ]

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
    let action = DeletedItems (true, [item], false)
    let model =
        testModelFromFs fs
        |> if isRedo then pushRedo action else id

    let testFunc =
        if isRedo
        then Action.redo fs progress
        else Action.delete fs progress [item]

    // part one: delete cancels correctly
    let modelAfterCancel = seqResultWithCancelTokenCallback (fs.CancelAfterWriteCount 2) testFunc model

    (
        let cancelledAction = DeletedItems (true, [item], true)
        let expected =
            { model with
                // no undo item because only items within the folder were deleted
                RedoStack = cancelledAction :: if isRedo then testModel.RedoStack else []
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
            then MainStatus.RedoAction (action, 1, 1)
            else MainStatus.ActionComplete action
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

    let testFunc = if permanent then Action.delete else Action.recycle
    let actual = seqResultWithCancelTokenCallback (fun ct -> ct.Cancel()) (testFunc fs progress [item]) model

    let expectedTotal = if permanent then 0 else 1
    let expected =
        model
        |> MainModel.withMessage (MainStatus.CancelledDelete (permanent, 0, expectedTotal))
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

    let actual = seqResult (Action.recycle fs progress [item]) model

    let expectedEx = FakeFileSystemErrors.cannotRecycleItemThatDoesNotFit 4L
    let expectedError = MainStatus.ActionError ("recycle", expectedEx)
    let expected =
        model
        |> MainModel.withError expectedError
        |> withNewCancelToken
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

    let actual = seqResult (Action.recycle fs progress [item]) model

    let expectedError = MainStatus.ActionError ("check folder content size", ex)
    let expected =
        model
        |> MainModel.withError expectedError
        |> withNewCancelToken
    assertAreEqual expected actual
    fs.Items |> shouldEqual expectedFs

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

    let actual = seqResult (Action.delete fs progress [item]) model

    let expectedErrorItems = [
        createPath "/c/folder/sub/sub file", ex
        createPath "/c/folder/sub", FakeFileSystemErrors.cannotDeleteNonEmptyFolder
        createPath "/c/folder", FakeFileSystemErrors.cannotDeleteNonEmptyFolder
    ]
    let expected =
        { model with
            // no undo item because only items within the folder were deleted
            RedoStack = []
        }
        |> MainModel.withError (MainStatus.DeleteError (true, expectedErrorItems, 5))
        |> withNewCancelToken
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
        driveWithSize 'c' 100L [
            file "file"
            file "other"
        ]
    ]
    let item = fs.Item "/c/file"
    fs.AddExnPath false ex item.Path
    let model = testModelFromFs fs
    let expectedFs = fs.Items

    let testFunc = if permanent then Action.delete else Action.recycle
    let actual = seqResult (testFunc fs progress [item]) model

    let expected =
        model
        |> MainModel.withError (MainStatus.DeleteError (permanent, [item.Path, ex], 1))
        |> withNewCancelToken
    assertAreEqual expected actual
    fs.Items |> shouldEqual expectedFs

[<TestCase(false)>]
[<TestCase(true)>]
let ``Recycle or Delete multiple items handles individual error and recycles or deletes other items and returns error`` permanent =
    let fs = FakeFileSystem [
        driveWithSize 'c' 100L [
            folder "folder" [
                file "sub file"
            ]
            file "error file"
            file "file"
            file "other"
        ]
    ]
    let items =
        [
            "/c/folder"
            "/c/error file"
            "/c/file"
        ] |> List.map fs.Item
    let errorItem = items.[1]
    fs.AddExnPath true ex errorItem.Path
    let model =
        testModelFromFs fs
        |> fun model -> { model with SelectedItems = items }

    let testFunc = if permanent then Action.delete else Action.recycle
    let actual = seqResult (testFunc fs progress items) model

    let expectedItems = [
        createFile "/c/error file"
        createFile "/c/other"
    ]
    let expectedDeletedItems = items |> List.except [errorItem]
    let expectedErrorItems = [errorItem.Path, ex]
    let expectedTotal = items.Length + if permanent then 1 else 0 // permanent delete enumerates items
    let expectedError = MainStatus.DeleteError (permanent, expectedErrorItems, expectedTotal)
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            SelectedItems = [errorItem]
            Cursor = 0
            CancelToken = CancelToken()
        }
        |> pushUndo (DeletedItems (permanent, expectedDeletedItems, false))
        |> popRedo
        |> MainModel.withError expectedError
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        driveWithSize 'c' 100L [
            file "error file"
            file "other"
        ]
    ]
    fs.RecycleBin |> shouldEqual (if permanent then [] else expectedDeletedItems)
