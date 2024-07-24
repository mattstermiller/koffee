module Koffee.MainActionTests_MoveCopy

open System
open NUnit.Framework
open FsUnitTyped
open Acadian.FSharp
open Koffee.Main

let withReg reg model =
    { model with Config = { model.Config with YankRegister = reg } }

let getCopyName = Action.getCopyName

let getCopyNames name num =
    List.init num (getCopyName name)

let assertAreEqual expected actual =
    assertAreEqualWith expected actual (fun comp -> comp.Config.MembersToIgnore.Remove "MainModel.History" |> ignore)

let putTypeCases () = [
    TestCaseData(Move)
    TestCaseData(Copy)
    TestCaseData(Shortcut)
]

let putTypeAndBoolCases () =
    putTypeCases () |> List.collect (fun c -> [
        TestCaseData(c.Arguments.[0], false)
        TestCaseData(c.Arguments.[0], true)
    ])

[<TestCaseSource(nameof putTypeCases)>]
let ``Register selected items with the same name returns error`` putType =
    let fs = FakeFileSystem [
        folder "source1" [
            file "file"
        ]
        folder "source2" [
            file "other file"
        ]
        folder "source3" [
            file "file"
        ]
    ]
    let selected = List.map createFile [
        "/c/source1/file"
        "/c/source2/other file"
        "/c/source3/file"
    ]
    let model =
        { testModel with
            Directory = selected
            Items = selected
            SelectedItems = selected
            SearchCurrent = Some ({ Search.Default with Terms = "file"; SubFolders = true })
        }

    Action.registerSelectedItems putType model
    |> shouldEqual (Error (MainStatus.CannotRegisterMultipleItemsWithSameName "file"))

[<TestCaseSource(nameof putTypeAndBoolCases)>]
let ``Put item in different folder with item of same name prompts for overwrite`` putType existingHidden =
    let destName = if putType = Shortcut then "file.lnk" else "file"
    let fs = FakeFileSystem [
        folder "other" [
            file "file"
        ]
        file "another file"
        fileWith (hide existingHidden) destName
        fileWith (hide true) "hidden"
    ]
    let src = fs.Item "/c/other/file"
    let dest = fs.Item ("/c/" + destName)
    let model = testModel |> withReg (Some (putType, [src.Ref]))
    let expectedItems = fs.ItemsIn "/c"

    let actual = seqResult (Action.put fs progress false) model

    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems |> List.filter (fun i -> i.Name <> "hidden")
            Cursor = 2
            InputMode = Some (Confirm (Overwrite (putType, [src, dest])))
            CancelToken = CancelToken()
        }
        |> withLocationOnHistory
    assertAreEqual expected actual
    fs.ItemsIn "/c" |> shouldEqual expectedItems

[<Test>]
let ``Put handles missing register item`` () =
    let src = createFile "/c/folder/file"
    let fs = FakeFileSystem [
        folder "folder" []
    ]
    let model = testModel |> withReg (Some (Move, [src.Ref]))

    let actual = seqResult (Action.put fs progress false) model

    let expectedEx = FakeFileSystemErrors.pathDoesNotExist src.Path
    let expected =
        model
        |> MainModel.withError (MainStatus.PutError (false, Move, [src.Path, expectedEx], 1))
        |> withNewCancelToken
    assertAreEqual expected actual

[<TestCaseSource(nameof putTypeCases)>]
let ``Put item handles file system errors`` putType =
    let fs = FakeFileSystem [
        folder "folder" [
            file "file"
        ]
    ]
    let src = fs.Item "/c/folder/file"
    let destPath = "/c/file" + (if putType = Shortcut then ".lnk" else "") |> createPath
    fs.AddExnPath true ex destPath
    let model = testModel |> withReg (Some (putType, [src.Ref]))
    let expectedFs = fs.Items

    let actual = seqResult (Action.put fs progress false) model

    let expected =
        model
        |> MainModel.withError (MainStatus.PutError (false, putType, [(src.Path, ex)], 1))
        |> withNewCancelToken
    assertAreEqual expected actual
    fs.Items |> shouldEqual expectedFs

[<TestCase(false, false)>]
[<TestCase(false, true)>]
[<TestCase(true, false)>]
[<TestCase(true, true)>]
let ``Put item in different folder calls file sys move or copy`` (copy: bool) (overwrite: bool) =
    let fs = FakeFileSystem [
        folder "folder" [
            fileWith (size 41L) "file"
        ]
        if overwrite then
            file "file"
    ]
    let src = fs.Item "/c/folder/file"
    let dest = createFile "/c/file"
    let putType = if copy then Copy else Move
    let model =
        testModel
        |> withReg (Some (putType, [src.Ref]))
        |> withHistoryPaths [itemHistoryPath src]

    let actual = seqResult (Action.put fs progress overwrite) model

    let intent = { createPutIntent [src] model.Location with Overwrite = overwrite }
    let actualPut = [{ createPutItem src dest.Path with DestExists = overwrite }]
    let expectedAction = PutItems (putType, intent, actualPut, false)
    let expectedItems = [
        createFolder "/c/folder"
        createFile "/c/file" |> size 41L
    ]
    let expected =
        { testModel with
            Directory = expectedItems
            Items = expectedItems
            Cursor = 1
            UndoStack = expectedAction :: testModel.UndoStack
            RedoStack = []
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (MainStatus.ActionComplete (expectedAction, testModel.PathFormat))
        |> withHistoryPaths [itemHistoryPath (if copy then src else dest)]
        |> withLocationOnHistory
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        folder "folder" [
            if copy then
                fileWith (size 41L) "file"
        ]
        fileWith (size 41L) "file"
    ]

[<TestCase(false, false)>]
[<TestCase(true, false)>]
[<TestCase(false, true)>]
[<TestCase(true, true)>]
let ``Put or redo put folder handles partial success by updating undo and setting error message``
        (copy: bool) (isRedo: bool) =
    let fs = FakeFileSystem [
        drive 'c' [
            folder "fruit" [
                folder "amazing" [
                    file "banana"
                ]
                file "apple"
                file "berry big error"
                file "cherry"
            ]
        ]
        drive 'd' [
            folder "other" []
        ]
    ]
    let src = fs.Item "/c/fruit"
    let dest = createPath "/d/fruit"
    let errorItem = createFile "/c/fruit/berry big error"
    fs.AddExn true ex "/d/fruit/berry big error"
    let putType = if copy then Copy else Move
    let intent = createPutIntent [src] dest.Parent
    let expectedPut = List.map (createPutItemFrom src.Path dest) [
        createFolder "/c/fruit"
        createFolder "/c/fruit/amazing"
        createFile "/c/fruit/amazing/banana"
        createFile "/c/fruit/apple"
        createFile "/c/fruit/cherry"
    ]
    let model =
        testModel
        |> MainModel.withLocation dest.Parent
        |> if isRedo
            then pushRedo (PutItems (putType, intent, [], false))
            else withReg (Some (putType, [src.Ref]))
        |> withHistoryPaths (historyPaths {
            "/d/other/"
            errorItem
            yield! expectedPut |> List.map (fun pi -> pi.Source, pi.ItemType = Folder)
        })
    let testFunc =
        if isRedo
        then Action.redo fs progress
        else Action.put fs progress false

    let actual = seqResult testFunc model

    let expectedAction = PutItems (putType, intent, expectedPut, false)
    let expectedItems = [
        createFolder "/d/fruit"
        createFolder "/d/other"
    ]
    let expectedError = MainStatus.PutError (false, putType, [(errorItem.Path, ex)], 6)
    let expected =
        { testModel with
            Directory = expectedItems
            Items = expectedItems
            Cursor = 0
            UndoStack = expectedAction :: testModel.UndoStack
            RedoStack = if isRedo then testModel.RedoStack else []
            CancelToken = CancelToken()
        }
        |> MainModel.withLocation dest.Parent
        |> MainModel.withError expectedError
        |> withHistoryPaths (historyPaths {
            if copy then
                yield! model.History.Paths
            else
                yield! model.History.Paths |> List.take 3
                yield! expectedPut |> List.skip 1 |> List.map (fun pi -> pi.Dest, pi.ItemType = Folder)
        })
        |> withLocationOnHistory
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        drive 'c' [
            folder "fruit" [
                if copy then
                    folder "amazing" [
                        file "banana"
                    ]
                    file "apple"
                    file "berry big error"
                    file "cherry"
                else
                    file "berry big error"
            ]
        ]
        drive 'd' [
            folder "fruit" [
                folder "amazing" [
                    file "banana"
                ]
                file "apple"
                file "cherry"
            ]
            folder "other" []
        ]
    ]

[<TestCase(false, false)>]
[<TestCase(true, false)>]
[<TestCase(false, true)>]
[<TestCase(true, true)>]
let ``Put or redo put enumerated folder moves or copies until canceled, then put or redo again resumes and redo merges undo item``
        (copy: bool) (isRedo: bool) =
    let fs = FakeFileSystem [
        drive 'c' [
            folder "fruit" [
                folder "amazing" [
                    file "banana"
                ]
                file "apple"
                file "cherry"
                fileWith (size 7L) "dewberry"
            ]
        ]
        drive 'd' []
    ]
    let src = fs.Item "/c/fruit"
    let dest = createFolder "/d/fruit"
    let putType = if copy then Copy else Move
    let intent = createPutIntent [src] dest.Path.Parent
    let actualPut = List.map (createPutItemFrom src.Path dest.Path) [
        createFolder "/c/fruit"
        createFolder "/c/fruit/amazing"
        createFile "/c/fruit/amazing/banana"
        createFile "/c/fruit/apple"
        createFile "/c/fruit/cherry"
        createFile "/c/fruit/dewberry" |> size 7L
    ]
    let regItem =
        if not isRedo
        then Some (putType, [src.Ref])
        else None
    let model =
        testModel
        |> withLocation "/d"
        |> withReg regItem
        |> if isRedo
            // add repeat to make sure redoIter stops when action is cancelled
            then pushRedo (PutItems (putType, intent, actualPut, false)) >> withRepeat 2
            else id
        |> withHistoryPaths (historyPaths {
            "/d/"
            actualPut.[0].Source, true
            actualPut.[1].Source, true
            actualPut.[2].Source, false
            actualPut.[3].Source, false
            actualPut.[4].Source, false
            actualPut.[5].Source, false
            actualPut.[3].Dest, false
        })
    let writesBeforeCancel = actualPut.Length - 2

    let testFunc isResume =
        if isRedo
        then Action.redo fs progress
        else Action.put fs progress isResume

    // part one: put or redo cancels correctly
    let modelAfterCancel = seqResultWithCancelTokenCallback (fs.CancelAfterWriteCount writesBeforeCancel) (testFunc false) model

    (
        let expectedAction = PutItems (putType, intent, actualPut |> List.take writesBeforeCancel, true)
        let expectedRedo =
            PutItems (putType, intent, actualPut |> List.skip writesBeforeCancel, true)
            :: (if isRedo then testModel.RedoStack else [])
        let expectedItems = [dest]
        let expected =
            { model with
                Directory = expectedItems
                Items = expectedItems
                Cursor = 0
                UndoStack = expectedAction :: testModel.UndoStack
                RedoStack = expectedRedo
            }
            |> MainModel.withMessage (MainStatus.CancelledPut (putType, false, writesBeforeCancel, actualPut.Length))
            |> withReg None
            |> withHistoryPaths (
                if copy then
                    model.History.Paths
                else
                    historyPaths {
                        "/d/"
                        actualPut.[0].Source, true
                        actualPut.[1].Dest, true
                        actualPut.[2].Dest, false
                        actualPut.[3].Dest, false
                        actualPut.[4].Source, false
                        actualPut.[5].Source, false
                    }
            )
        assertAreEqual expected modelAfterCancel
        fs.ItemsShouldEqual [
            drive 'c' [
                folder "fruit" [
                    if copy then
                        folder "amazing" [
                            file "banana"
                        ]
                        file "apple"
                    file "cherry"
                    fileWith (size 7L) "dewberry"
                ]
            ]
            drive 'd' [
                folder "fruit" [
                    folder "amazing" [
                        file "banana"
                    ]
                    file "apple"
                ]
            ]
        ]
        printfn "part one: cancellation completed successfully"
    )

    // simulate external program creating new file in destination with same relative path as a remaining file
    let putItemWithConflict = actualPut.[5]
    fs.Create File putItemWithConflict.Dest |> assertOk

    // part two: put or redo again completes the operation and merges undo item
    // - put should merge with folder created in part one and overwrite items
    // - redo should return partial success with error on new item since original operation was not an overwrite
    // - redo copy should not copy completed items again
    let actual =
        { modelAfterCancel with RepeatCommand = None }
        |> withReg regItem
        |> seqResult (testFunc true)

    let expectedPut forMergedUndoItem = [
        actualPut.[0] |> applyIf (not isRedo) withDestExists
        if not isRedo && copy then
            yield! actualPut.[1..3] |> List.map withDestExists
        else if forMergedUndoItem then
            yield! actualPut.[1..3]
        actualPut.[4]
        if not isRedo then
            actualPut.[5] |> withDestExists
    ]
    let expectedStatusPut = expectedPut false
    let expectedStatusAction = PutItems (putType, { intent with Overwrite = true }, expectedStatusPut, false)
    let expectedMergedAction = PutItems (putType, intent, expectedPut true, false)
    let expectedItems = [dest]
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            Cursor = 0
            UndoStack = expectedMergedAction :: testModel.UndoStack
            RedoStack = if isRedo then testModel.RedoStack else []
            RepeatCommand = None
            CancelToken = CancelToken()
        }
        |> withReg None
        |> if isRedo then
                let expectedError = (putItemWithConflict.Source, RedoPutBlockedByExistingItemException() :> exn)
                MainModel.withError (MainStatus.PutError (false, putType, [expectedError], expectedStatusPut.Length))
            else
                MainModel.withMessage (MainStatus.ActionComplete (expectedStatusAction, testModel.PathFormat))
        |> withHistoryPaths (
            if copy then
                model.History.Paths
            else
                historyPaths {
                    "/d/"
                    if isRedo then
                        actualPut.[0].Source, true
                    else
                        actualPut.[0].Dest, true
                    actualPut.[1].Dest, true
                    actualPut.[2].Dest, false
                    actualPut.[3].Dest, false
                    actualPut.[4].Dest, false
                    if isRedo then
                        actualPut.[5].Source, false
                    else
                        actualPut.[5].Dest, false
                }
        )
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        drive 'c' [
            if copy then
                folder "fruit" [
                    folder "amazing" [
                        file "banana"
                    ]
                    file "apple"
                    file "cherry"
                    fileWith (size 7L) "dewberry"
                ]
            else if isRedo then
                folder "fruit" [
                    fileWith (size 7L) "dewberry"
                ]
        ]
        drive 'd' [
            folder "fruit" [
                folder "amazing" [
                    file "banana"
                ]
                file "apple"
                file "cherry"
                fileWith (if isRedo then id else size 7L) "dewberry"
            ]
        ]
    ]

[<TestCase(false, false)>]
[<TestCase(true, false)>]
[<TestCase(false, true)>]
[<TestCase(true, true)>]
let ``Put or redo put enumerated folder handles partial success with cancellation by updating undo and redo and setting error message``
        (copy: bool) (isRedo: bool) =
    let fs = FakeFileSystem [
        drive 'c' [
            folder "fruit" [
                folder "amazing" [
                    file "banana"
                ]
                folder "bad" [
                    file "grapefruit"
                ]
                file "cherry"
                file "dewberry"
            ]
        ]
        drive 'd' []
    ]
    let src = fs.Item "/c/fruit"
    let dest = createPath "/d/fruit"
    let putType = if copy then Copy else Move
    let intent = createPutIntent [src] dest.Parent
    let actualPut = List.map (createPutItemFrom src.Path dest) [
        createFolder "/c/fruit"
        createFolder "/c/fruit/amazing"
        createFile "/c/fruit/amazing/banana"
        createFolder "/c/fruit/bad"
        createFile "/c/fruit/bad/grapefruit"
        createFile "/c/fruit/cherry"
        createFile "/c/fruit/dewberry"
    ]
    let errorItem = actualPut.[4]
    fs.AddExnPath true ex errorItem.Dest
    let regItem =
        if not isRedo
        then Some (putType, [src.Ref])
        else None
    let model =
        testModel
        |> withLocation "/d"
        |> withReg regItem
        |> if isRedo
            then pushRedo (PutItems (putType, intent, actualPut, false))
            else id
        |> withHistoryPaths (historyPaths {
            "/d/"
            yield! actualPut |> List.map (fun pi -> pi.Source, pi.ItemType = Folder)
        })
    let writesBeforeCancel = actualPut.Length - 1

    let testFunc =
        if isRedo
        then Action.redo fs progress
        else Action.put fs progress false

    let actual = seqResultWithCancelTokenCallback (fs.CancelAfterWriteCount writesBeforeCancel) testFunc model

    let expectedUnsuccessfulItems = [
        errorItem
        actualPut.[6]
    ]
    let expectedSuccessItems = actualPut |> List.except expectedUnsuccessfulItems
    let expectedUndoAction = PutItems (putType, intent, expectedSuccessItems, true)
    let expectedRedoAction = PutItems (putType, intent, expectedUnsuccessfulItems, true)
    let expectedItems = [createFolder "/d/fruit"]
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            Cursor = 0
            UndoStack = expectedUndoAction :: testModel.UndoStack
            RedoStack = expectedRedoAction :: if isRedo then testModel.RedoStack else []
        }
        |> MainModel.withError (MainStatus.PutError (false, putType, [errorItem.Source, ex], actualPut.Length))
        |> withReg None
        |> withHistoryPaths (historyPaths {
            "/d/"
            actualPut.[0].Source, true
            if copy then
                actualPut.[1].Source, true
                actualPut.[2].Source, false
            else
                actualPut.[1].Dest, true
                actualPut.[2].Dest, false
            actualPut.[3].Source, true
            actualPut.[4].Source, false
            if copy then
                actualPut.[5].Source, false
            else
                actualPut.[5].Dest, false
            actualPut.[6].Source, false
        })
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        drive 'c' [
            folder "fruit" [
                if copy then
                    folder "amazing" [
                        file "banana"
                    ]
                folder "bad" [
                    file "grapefruit"
                ]
                if copy then
                    file "cherry"
                file "dewberry"
            ]
        ]
        drive 'd' [
            folder "fruit" [
                folder "amazing" [
                    file "banana"
                ]
                folder "bad" []
                file "cherry"
            ]
        ]
    ]

[<TestCase(false)>]
[<TestCase(true)>]
let ``Put enumerated folder does nothing when canceled immediately`` (copy: bool) =
    let fs = FakeFileSystem [
        drive 'c' [
            folder "fruit" [
                folder "amazing" [
                    file "banana"
                ]
                file "apple"
                file "cherry"
            ]
        ]
        drive 'd' []
    ]
    let src = fs.Item "/c/fruit"
    let putType = if copy then Copy else Move
    let model = testModel |> withLocation "/d" |> withReg (Some (putType, [src.Ref]))
    let expectedFs = fs.Items

    let actual = seqResultWithCancelTokenCallback (fun ct -> ct.Cancel()) (Action.put fs progress false) model

    let expected = model |> MainModel.withMessage (MainStatus.CancelledPut (putType, false, 0, 0))
    assertAreEqual expected actual
    fs.ItemsShouldEqualList expectedFs

[<TestCase(false)>]
[<TestCase(true)>]
let ``Put folder where dest has folder with same name merges correctly`` (copy: bool) =
    let srcTree =
        folder "fruit" [
            folder "bushes" [
                file "blueberry"
            ]
            folder "trees" [
                fileWith (size 7L) "apple"
                file "banana"
            ]
            folder "vines" []
            file "tomato"
        ]
    let fs = FakeFileSystem [
        srcTree
        folder "dest" [
            folder "fruit" [
                folder "trees" [
                    file "apple"
                    file "orange"
                ]
                folder "vines" [
                    file "grapes"
                ]
            ]
        ]
    ]
    let src = fs.Item "/c/fruit"
    let dest = createFolder "/c/dest/fruit"
    let putType = if copy then Copy else Move
    let model = testModel |> withLocation "/c/dest" |> withReg (Some (putType, [src.Ref]))

    let actual = seqResult (Action.put fs progress true) model

    let intent = { createPutIntent [src] model.Location with Overwrite = true }
    let createPutItem = createPutItemFrom src.Path dest.Path
    let expectedPut = [
        createPutItem (createFolder "/c/fruit") |> withDestExists
        createPutItem (createFolder "/c/fruit/bushes")
        if copy then
            createPutItem (createFile "/c/fruit/bushes/blueberry")
        createPutItem (createFolder "/c/fruit/trees") |> withDestExists
        createPutItem (createFile "/c/fruit/trees/apple" |> size 7L) |> withDestExists
        createPutItem (createFile "/c/fruit/trees/banana")
        createPutItem (createFolder "/c/fruit/vines") |> withDestExists
        createPutItem (createFile "/c/fruit/tomato")
    ]
    let expectedAction = PutItems (putType, intent, expectedPut, false)
    let expectedItems = [dest]
    let expected =
        { testModel with
            Directory = expectedItems
            Items = expectedItems
            UndoStack = expectedAction :: testModel.UndoStack
            RedoStack = []
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (MainStatus.ActionComplete (expectedAction, testModel.PathFormat))
        |> withLocation "/c/dest"
        |> withLocationOnHistory
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        if copy then
            srcTree
        folder "dest" [
            folder "fruit" [
                folder "bushes" [
                    file "blueberry"
                ]
                folder "trees" [
                    fileWith (size 7L) "apple"
                    file "banana"
                    file "orange"
                ]
                folder "vines" [
                    file "grapes"
                ]
                file "tomato"
            ]
        ]
    ]

[<TestCaseSource(nameof putTypeCases)>]
let ``Put items from different parents works correctly`` putType =
    let fs = FakeFileSystem [
        folder "source1" [
            file "file1"
            file "other"
        ]
        folder "source2" [
            file "file2"
        ]
        folder "source3" [
            file "file3"
        ]
    ]
    let sources = List.map createFile [
        "/c/source1/file1"
        "/c/source2/file2"
        "/c/source3/file3"
    ]
    let model =
        testModel
        |> withReg (Some (putType, sources |> List.map (fun i -> i.Ref)))
        |> withHistoryPaths (sources |> List.map itemHistoryPath)

    let actual = seqResult (Action.put fs progress false) model

    let destParent = model.Location
    let destPath name = destParent.Join (name + if putType = Shortcut then ".lnk" else "")
    let destItems =
        sources |> List.map (fun item ->
            let path = destPath item.Name
            { item with Path = path; Name = path.Name }
        )
    let intent = createPutIntent sources destParent
    let actualPut = sources |> List.map (fun src -> createPutItem src (destPath src.Name))
    let expectedAction = PutItems (putType, intent, actualPut, false)
    let expectedItems = [
        createFolder "/c/source1"
        createFolder "/c/source2"
        createFolder "/c/source3"
        yield! destItems
    ]
    let expected =
        { testModel with
            Directory = expectedItems
            Items = expectedItems
            Cursor = sources.Length
            SelectedItems = destItems
            UndoStack = expectedAction :: testModel.UndoStack
            RedoStack = []
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (MainStatus.ActionComplete (expectedAction, testModel.PathFormat))
        |> withHistoryPaths ((if putType = Move then destItems else sources) |> List.map itemHistoryPath)
        |> withLocationOnHistory
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        folder "source1" [
            if not (putType = Move) then
                file "file1"
            file "other"
        ]
        folder "source2" [
            if not (putType = Move) then
                file "file2"
        ]
        folder "source3" [
            if not (putType = Move) then
                file "file3"
        ]
        if putType = Shortcut then
            file "file1.lnk"
            file "file2.lnk"
            file "file3.lnk"
        else
            file "file1"
            file "file2"
            file "file3"
    ]

[<TestCaseSource(nameof putTypeAndBoolCases)>]
let ``Put in location items with the same name from different parents returns error`` putType destExists =
    let fs = FakeFileSystem [
        folder "source1" [
            file "file"
        ]
        folder "source2" [
            file "other"
        ]
        folder "source3" [
            file "file"
        ]
        if destExists then
            file ("file" + if putType = Shortcut then ".lnk" else "")
    ]
    let items = List.map createFile [
        "/c/source1/file"
        "/c/source2/other"
        "/c/source3/file"
    ]
    let itemRefs = items |> List.map (fun i -> i.Ref)
    let model = testModel |> withHistoryPaths (items |> List.map itemHistoryPath)
    let expectedFs = fs.Items

    // test putInLocation because it is used by dropIn and paste where the item list is not restricted.
    // `put` could not trigger this because registering items with the same name is not allowed.
    let actual = seqResult (Action.putInLocation fs progress false false putType itemRefs) model

    let expectedError = MainStatus.CannotPutMultipleItemsWithSameName (putType, "file")
    let expected = model |> MainModel.withError expectedError
    assertAreEqual expected actual
    fs.Items |> shouldEqual expectedFs

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo put enumerated folder moves or deletes until canceled, then undo again resumes and merges redo item`` wasCopy =
    let itemTree =
        folder "fruit" [
            folder "amazing" [
                file "banana"
            ]
            file "apple"
            file "cherry"
        ]
    let fs = FakeFileSystem [
        drive 'c' [
            if wasCopy then
                itemTree
        ]
        drive 'd' [
            itemTree
            file "other"
        ]
    ]
    let dest = fs.Item "/d/fruit"
    let original = createFolder "/c/fruit"
    let putItem = createPutIntent [original] dest.Path.Parent
    let actualPut = List.map (createPutItemFrom original.Path dest.Path) [
        createFolder "/c/fruit"
        createFolder "/c/fruit/amazing"
        createFile "/c/fruit/amazing/banana"
        createFile "/c/fruit/apple"
        createFile "/c/fruit/cherry"
    ]
    let putType = if wasCopy then Copy else Move
    let action = PutItems (putType, putItem, actualPut, false)
    let model =
        testModel
        |> withLocation "/d"
        |> pushUndo action
        |> withRepeat 2 // make sure undoIter stops when action is cancelled
        |> withHistoryPaths (historyPaths {
            actualPut.[0].Dest, true
            actualPut.[1].Dest, true
            actualPut.[2].Dest, false
            actualPut.[3].Dest, false
            actualPut.[4].Dest, false
        })
    let writesBeforeCancel = actualPut.Length - 1

    // part one: undo cancels correctly
    let modelAfterCancel =
        seqResultWithCancelTokenCallback (fs.CancelAfterWriteCount writesBeforeCancel) (Action.undo fs progress) model

    let expectedPutBeforeCancel, expectedPutAfterCancel =
        actualPut
        |> applyIf wasCopy List.rev // undo copy deletes items in reverse order
        |> List.splitAt writesBeforeCancel
        |> applyIf wasCopy (fun (before, after) -> (before |> List.rev, after |> List.rev))

    (
        let expectedUndoAction = PutItems (putType, putItem, expectedPutAfterCancel, true)
        let expectedRedoAction = PutItems (putType, putItem, expectedPutBeforeCancel, true)
        let expectedItems =
            if wasCopy then
                [
                    dest
                    createFile "/d/other"
                ]
            else
                [original]
        let expected =
            { model with
                Directory = expectedItems
                Items = expectedItems
                Cursor = 0
                UndoStack = expectedUndoAction :: testModel.UndoStack
                RedoStack = expectedRedoAction :: testModel.RedoStack
            }
            |> MainModel.withMessage (MainStatus.CancelledPut (putType, true, writesBeforeCancel, actualPut.Length))
            // undo copy does not open a path, only refreshes if current path is destination
            |> applyIf (not wasCopy) (MainModel.withPushedLocation original.Path.Parent)
            |> withHistoryPaths (historyPaths {
                actualPut.[0].Dest, true
                if not wasCopy then
                    actualPut.[1].Source, true
                    actualPut.[2].Source, false
                    actualPut.[3].Source, false
                    actualPut.[4].Dest, false
            })
            |> withLocationOnHistory
        assertAreEqual expected modelAfterCancel
        fs.ItemsShouldEqual [
            drive 'c' [
                if wasCopy then
                    itemTree
                else
                    folder "fruit" [
                        folder "amazing" [
                            file "banana"
                        ]
                        file "apple"
                    ]
            ]
            drive 'd' [
                folder "fruit" [
                    if not wasCopy then // undo copy deletes in reverse, so the only item left should be parent folder
                        file "cherry"
                ]
                file "other"
            ]
        ]
        printfn "part one: cancellation completed successfully"
    )

    // part two: undo again resumes and completes the operation
    let actual =
        { modelAfterCancel with RepeatCommand = None }
        |> seqResult (Action.undo fs progress)

    let expectedStatusAction = PutItems (putType, putItem, expectedPutAfterCancel, false)
    let expectedRedoAction = PutItems (putType, putItem, actualPut, false)
    let expectedItems =
        if wasCopy
        then [createFile "/d/other"]
        else [original]
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            Cursor = 0
            UndoStack = model.UndoStack.Tail
            RedoStack = expectedRedoAction :: model.RedoStack
            RepeatCommand = None
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (MainStatus.UndoAction (expectedStatusAction, model.PathFormat, 1, 1))
        // undo copy does not open a path, only refreshes if current path is destination
        |> applyIf (not wasCopy) (MainModel.withPushedLocation original.Path.Parent)
        |> withHistoryPaths (historyPaths {
            if not wasCopy then
                actualPut.[0].Source, true
                actualPut.[1].Source, true
                actualPut.[2].Source, false
                actualPut.[3].Source, false
                actualPut.[4].Source, false
        })
        |> withLocationOnHistory
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        drive 'c' [
            itemTree
        ]
        drive 'd' [
            file "other"
        ]
    ]

[<TestCase(false)>]
[<TestCase(true)>]
let ``Redo put performs move or copy of intent instead of actual`` (copy: bool) =
    let fs = FakeFileSystem [
        drive 'c' [
            folder "folder" [
                file "file"
                file "new" // simulates new file created externally in source folder
            ]
        ]
        drive 'd' [
            folder "dest" [
                folder "another" []
            ]
        ]
    ]
    let src = createFolder "/c/folder"
    let dest = createPath "/d/dest/folder"
    let intent = createPutIntent [src] dest.Parent
    let actualPut = List.map (createPutItemFrom src.Path dest) [
        createFolder "/c/folder"
        createFile "/c/folder/file"
    ]
    let putType = if copy then Copy else Move
    let action = PutItems (putType, intent, actualPut, false)
    let model = testModel |> withLocation "/d" |> pushRedo action

    let actual = seqResult (Action.redo fs progress) model

    let expectedActual = actualPut @ [
        createFile "/c/folder/new" |> createPutItemFrom src.Path dest
    ]
    let expectedAction = PutItems (putType, intent, expectedActual, false)
    let expectedItems = [
        createFolder "/d/dest/another"
        createFolder "/d/dest/folder"
    ]
    let expected =
        { (model |> MainModel.withPushedLocation dest.Parent) with
            Directory = expectedItems
            Items = expectedItems
            Cursor = 1
            UndoStack = expectedAction :: model.UndoStack
            RedoStack = model.RedoStack.Tail
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (MainStatus.RedoAction (expectedAction, testModel.PathFormat, 1, 1))
        |> withLocationOnHistory
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        drive 'c' [
            if copy then
                folder "folder" [
                    file "file"
                    file "new"
                ]
        ]
        drive 'd' [
            folder "dest" [
                folder "another" []
                folder "folder" [
                    file "file"
                    file "new"
                ]
            ]
        ]
    ]

[<TestCaseSource(nameof putTypeCases)>]
let ``Redo put item that was not an overwrite when path is occupied returns error`` putType =
    let fs = FakeFileSystem [
        folder "dest" [
            folder "other" []
            if putType = Shortcut then
                file "put.lnk"
            else
                folder "put" []
        ]
        folder "put" [
            file "file"
            file "other"
        ]
    ]
    let src = createFolder "/c/put"
    let dest = createPath ("/c/dest/put" + if putType = Shortcut then ".lnk" else "")
    let intent = createPutIntent [src] dest.Parent
    let actualPut =
        List.map (createPutItemFrom src.Path dest) [
            src
            if putType <> Shortcut then
                createFile "/c/put/file"
                createFile "/c/put/other"
        ]
    let action = PutItems (putType, intent, actualPut, false)
    let model = testModel |> pushRedo action
    let expectedFs = fs.Items

    let actual = seqResult (Action.redo fs progress) model

    let expectedError = MainStatus.PutError (false, putType, [src.Path, RedoPutBlockedByExistingItemException() :> exn], 1)
    let expectedItems = [
        createFolder "/c/dest/other"
        if putType = Shortcut then
            createFile "/c/dest/put.lnk"
        else
            createFolder "/c/dest/put"
    ]
    let expected =
        { (model |> MainModel.withPushedLocation dest.Parent) with
            Directory = expectedItems
            Items = expectedItems
            RedoStack = model.RedoStack.Tail
            CancelToken = CancelToken()
        }
        |> MainModel.withError expectedError
        |> withLocationOnHistory
    assertAreEqual expected actual
    fs.Items |> shouldEqual expectedFs


// move tests

[<TestCase(false, false)>]
[<TestCase(true, false)>]
[<TestCase(true, true)>]
let ``Put folder to move deletes source folder after enumerated move and updates history`` enumerated deleteError =
    let fs = FakeFileSystem [
        folder "dest" [
            if enumerated then
                folder "folder" []
        ]
        folder "folder" [
            folder "sub" []
            file "file"
        ]
    ]
    let src = fs.Item "/c/folder"
    let dest = createFolder "/c/dest/folder"
    if enumerated && deleteError then
        fs.AddExnPath true ex src.Path
    let model =
        testModel
        |> MainModel.withLocation dest.Path.Parent
        |> withReg (Some (Move, [src.Ref]))
        |> withHistoryPaths (historyPaths {
            "/c/folder/file"
            "/c/dest2/unrelated"
            "/c/folder/sub/"
            src
        })
    let createPutItem = createPutItemFrom src.Path dest.Path
    let expectedPut = [
        src |> createPutItem |> applyIf enumerated withDestExists
        if enumerated then
            createFolder "/c/folder/sub" |> createPutItem
            createFile "/c/folder/file" |> createPutItem
    ]

    let actual = seqResult (Action.put fs progress enumerated) model

    let expectedIntent = { createPutIntent [src] model.Location with Overwrite = enumerated }
    let expectedAction = PutItems (Move, expectedIntent, expectedPut, false)
    let expectedItems = [dest]
    let expected =
        { testModel with
            Directory = expectedItems
            Items = expectedItems
            UndoStack = expectedAction :: testModel.UndoStack
            RedoStack = []
            CancelToken = CancelToken()
        }
        |> MainModel.withLocation dest.Path.Parent
        |> if enumerated && deleteError
            then MainModel.withError (MainStatus.CouldNotDeleteMoveSource (src.Name, ex))
            else MainModel.withMessage (MainStatus.ActionComplete (expectedAction, testModel.PathFormat))
        |> withHistoryPaths (historyPaths {
            "/c/dest/folder/file"
            "/c/dest2/unrelated"
            "/c/dest/folder/sub/"
            dest
        })
        |> withLocationOnHistory
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        folder "dest" [
            folder "folder" [
                folder "sub" []
                file "file"
            ]
        ]
        if enumerated && deleteError then
            folder "folder" []
    ]

[<Test>]
let ``Put item to move in same folder returns error``() =
    let fs = FakeFileSystem [
        file "file"
    ]
    let src = fs.Item "/c/file"
    let model = testModel |> withReg (Some (Move, [src.Ref]))
    let expectedFs = fs.Items

    let actual = seqResult (Action.put fs progress false) model

    let expected = model |> MainModel.withError MainStatus.CannotMoveToSameFolder
    assertAreEqual expected actual
    fs.Items |> shouldEqual expectedFs

// undo move tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo move item moves it back`` curPathDifferent =
    let fs = FakeFileSystem [
        folder "dest" [
            file "file"
        ]
        folder "other" []
        file "another"
    ]
    let moved = fs.Item "/c/dest/file"
    let original = createFile "/c/file"
    let intent, putItem = createPutIntentAndItem original moved.Path
    let action = PutItems (Move, intent, [putItem], false)
    let location = if curPathDifferent then "/c/other" else "/c"
    let model = testModel |> withLocation location |> pushUndo action

    let actual = seqResult (Action.undo fs progress) model

    let expectedItems = [
        createFolder "/c/dest"
        createFolder "/c/other"
        createFile "/c/another"
        createFile "/c/file"
    ]
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            Cursor = 3
            UndoStack = model.UndoStack.Tail
            RedoStack = action :: model.RedoStack
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (MainStatus.UndoAction (action, model.PathFormat, 1, 1))
        |> withLocation "/c"
        |> withLocationOnHistory
        |> withBackIf curPathDifferent (model.Location, 0)
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        folder "dest" []
        folder "other" []
        file "another"
        file "file"
    ]

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo move of enumerated folder deletes original dest folder when empty`` destFolderEmpty =
    let fs = FakeFileSystem [
        drive 'c' [
            folder "another" []
        ]
        drive 'd' [
            folder "moved" [
                file "file"
                if not destFolderEmpty then
                    file "new"
                file "other"
            ]
        ]
    ]
    let moved = fs.Item "/d/moved"
    let original = createFolder "/c/moved"
    let intent = createPutIntent [original] moved.Path.Parent
    let actualMoved = List.map (createPutItemFrom original.Path moved.Path) [
        original
        createFile "/c/moved/file"
        createFile "/c/moved/other"
    ]
    let action = PutItems (Move, intent, actualMoved, false)
    let model =
        testModel
        |> withLocation "/d"
        |> pushUndo action
        |> withHistoryPaths (historyPaths {
            moved
            actualMoved.[1].Dest, false
            actualMoved.[2].Source, false
            if not destFolderEmpty then
                moved.Path.Join "new", false
        })

    let actual = seqResult (Action.undo fs progress) model

    let expectedItems = [
        createFolder "/c/another"
        createFolder "/c/moved"
    ]
    let expected =
        { (model |> MainModel.withPushedLocation original.Path.Parent) with
            Directory = expectedItems
            Items = expectedItems
            Cursor = 1
            UndoStack = model.UndoStack.Tail
            RedoStack = action :: model.RedoStack
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (MainStatus.UndoAction (action, testModel.PathFormat, 1, 1))
        |> withHistoryPaths (historyPaths {
            original
            actualMoved.[1].Source, false
            actualMoved.[2].Source, false
            if not destFolderEmpty then
                original.Path.Join "new", false
        })
        |> withLocationOnHistory
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        drive 'c' [
            folder "another" []
            folder "moved" [
                file "file"
                file "other"
            ]
        ]
        drive 'd' [
            if not destFolderEmpty then
                folder "moved" [
                    file "new"
                ]
        ]
    ]

[<Test>]
let ``Undo move copies back items that were overwrites and recreates empty folders`` () =
    let fs = FakeFileSystem [
        folder "dest" [
            folder "moved" [
                folder "folder" [
                    file "sub"
                ]
                file "file"
                file "other"
            ]
        ]
    ]
    let original = createFolder "/c/moved"
    let moved = fs.Item "/c/dest/moved"
    let intent = { createPutIntent [original] moved.Path.Parent with Overwrite = true }
    let createPutItem = createPutItemFrom original.Path moved.Path
    let actualMoved = [
        createFolder "/c/moved" |> createPutItem |> withDestExists
        createFolder "/c/moved/folder" |> createPutItem |> withDestExists
        createFile "/c/moved/file" |> createPutItem |> withDestExists
        createFile "/c/moved/other" |> createPutItem
    ]
    let action = PutItems (Move, intent, actualMoved, false)
    let model =
        testModel
        |> pushUndo action
        |> withHistoryPaths (historyPaths {
            actualMoved.[0].Dest, true
            actualMoved.[1].Dest, true
            actualMoved.[2].Dest, false
            actualMoved.[3].Dest, false
        })

    let actual = seqResult (Action.undo fs progress) model

    let expectedItems = [
        createFolder "/c/dest"
        createFolder "/c/moved"
    ]
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            Cursor = 1
            UndoStack = model.UndoStack.Tail
            RedoStack = action :: testModel.RedoStack
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (MainStatus.UndoAction (action, testModel.PathFormat, 1, 1))
        |> withHistoryPaths (historyPaths {
            yield! model.History.Paths |> List.take 3
            actualMoved.[3].Source, false
        })
        |> withLocationOnHistory
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        folder "dest" [
            folder "moved" [
                folder "folder" [
                    file "sub"
                ]
                file "file"
            ]
        ]
        folder "moved" [
            folder "folder" []
            file "file"
            file "other"
        ]
    ]

[<TestCase(false, false)>]
[<TestCase(false, true)>]
[<TestCase(true, false)>]
[<TestCase(true, true)>]
let ``Undo move handles partial success by updating redo and setting error message`` errorCausedByExisting destExisted =
    let fs = FakeFileSystem [
        drive 'c' [
            folder "another" []
            if errorCausedByExisting then
                folder "moved" [
                    fileWith (size 7L) "file"
                ]
        ]
        drive 'd' [
            folder "moved" [
                file "file"
                file "other"
            ]
        ]
    ]
    let errorItem = createFile "/d/moved/file"
    if not errorCausedByExisting then
        fs.AddExnPath false ex errorItem.Path
    let destPath = createPath "/d/moved"
    let original = createFolder "/c/moved"
    let intent = { createPutIntent [original] destPath.Parent with Overwrite = destExisted }
    let actualMoved = [
        createPutItem original destPath |> applyIf destExisted withDestExists
        yield! List.map (createPutItemFrom original.Path destPath) [
            createFile "/c/moved/file"
            createFile "/c/moved/other"
        ]
    ]
    let action = PutItems (Move, intent, actualMoved, false)
    let model =
        testModel
        |> withLocation "/d"
        |> pushUndo action
        |> withHistoryPaths (historyPaths {
            "/d/unrelated"
            actualMoved.[0].Dest, true
            actualMoved.[1].Dest, false
            actualMoved.[2].Dest, false
        })

    let actual = seqResult (Action.undo fs progress) model

    let expectedItems = [
        createFolder "/c/another"
        createFolder "/c/moved"
    ]
    let expectedError = if errorCausedByExisting then UndoMoveBlockedByExistingItemException() :> exn else ex
    let expectedAction = PutItems (Move, intent, [actualMoved.[0]; actualMoved.[2]], false)
    let expected =
        model
        |> MainModel.withPushedLocation original.Path.Parent
        |> fun model ->
            { model with
                Directory = expectedItems
                Items = expectedItems
                Cursor = 1
                UndoStack = model.UndoStack.Tail
                RedoStack = expectedAction :: testModel.RedoStack
                CancelToken = CancelToken()
            }
        |> MainModel.withError (MainStatus.PutError (true, Move, [errorItem.Path, expectedError], actualMoved.Length))
        |> withHistoryPaths (historyPaths {
            yield! model.History.Paths |> List.take 3
            actualMoved.[2].Source, false
        })
        |> withLocationOnHistory
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        drive 'c' [
            folder "another" []
            folder "moved" [
                if errorCausedByExisting then
                    fileWith (size 7L) "file"
                file "other"
            ]
        ]
        drive 'd' [
            folder "moved" [
                file "file"
            ]
        ]
    ]

[<Test>]
let ``Undo move enumerated folder handles partial success with cancellation by updating undo and redo and setting error message`` () =
    let itemTree =
        folder "fruit" [
            folder "amazing" [
                file "banana"
            ]
            folder "bad" [
                file "grapefruit"
            ]
            file "cherry"
            file "dewberry"
        ]
    let fs = FakeFileSystem [
        drive 'c' []
        drive 'd' [
            itemTree
            file "other"
        ]
    ]
    let original = createFolder "/c/fruit"
    let dest = fs.Item "/d/fruit"
    let intent = createPutIntent [original] dest.Path.Parent
    let actualPut = List.map (createPutItemFrom original.Path dest.Path) [
        createFolder "/c/fruit"
        createFolder "/c/fruit/amazing"
        createFile "/c/fruit/amazing/banana"
        createFolder "/c/fruit/bad"
        createFile "/c/fruit/bad/grapefruit"
        createFile "/c/fruit/cherry"
        createFile "/c/fruit/dewberry"
    ]
    let action = PutItems (Move, intent, actualPut, false)
    let errorItem = actualPut.[4] // bad/grapefruit
    fs.AddExnPath true ex errorItem.Dest
    let model =
        testModel
        |> withLocation "/d"
        |> pushUndo action
        |> withHistoryPaths (historyPaths {
            actualPut.[0].Dest, true
            actualPut.[1].Dest, true
            actualPut.[2].Dest, false
            actualPut.[3].Dest, true
            actualPut.[4].Dest, false
            actualPut.[5].Dest, false
            actualPut.[6].Dest, false
        })
    let writesBeforeCancel = actualPut.Length - 1

    let actual = seqResultWithCancelTokenCallback (fs.CancelAfterWriteCount writesBeforeCancel) (Action.undo fs progress) model

    let expectedUnsuccessful = [
        actualPut.[4]
        actualPut.[6]
    ]
    let expectedUndoAction = PutItems (Move, intent, expectedUnsuccessful, true)
    let expectedRedoAction = PutItems (Move, intent, actualPut |> List.except expectedUnsuccessful, true)
    let expectedItems = [original]
    let expected =
        { model with
            Directory = expectedItems |> sortByPath
            Items = expectedItems
            Cursor = 0
            UndoStack = expectedUndoAction :: testModel.UndoStack
            RedoStack = expectedRedoAction :: testModel.RedoStack
        }
        |> MainModel.withError (MainStatus.PutError (true, Move, [actualPut.[4].Dest, ex], actualPut.Length))
        |> MainModel.withPushedLocation original.Path.Parent
        |> withHistoryPaths (historyPaths {
            actualPut.[0].Dest, true
            actualPut.[1].Source, true
            actualPut.[2].Source, false
            actualPut.[3].Dest, true
            actualPut.[4].Dest, false
            actualPut.[5].Source, false
            actualPut.[6].Dest, false
        })
        |> withLocationOnHistory
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        drive 'c' [
            folder "fruit" [
                folder "amazing" [
                    file "banana"
                ]
                folder "bad" []
                file "cherry"
            ]
        ]
        drive 'd' [
            folder "fruit" [
                folder "bad" [
                    file "grapefruit"
                ]
                file "dewberry"
            ]
            file "other"
        ]
    ]

[<Test>]
let ``Undo move item when previous path is occupied returns error``() =
    let fs = FakeFileSystem [
        folder "dest" [
            file "file"
        ]
        folder "other" []
        fileWith (size 7L) "file"
    ]
    let moved = fs.Item "/c/dest/file"
    let original = createFile "/c/file"
    let intent, putItem = createPutIntentAndItem original moved.Path
    let action = PutItems (Move, intent, [putItem], false)
    let model = testModel |> withLocation "/c/other" |> pushUndo action
    let expectedFs = fs.Items

    let actual = seqResult (Action.undo fs progress) model

    let expectedError = MainStatus.PutError (true, Move, [(moved.Path, UndoMoveBlockedByExistingItemException() :> exn)], 1)
    let expected = model |> MainModel.withError expectedError |> popUndo |> withNewCancelToken
    assertAreEqual expected actual
    fs.Items |> shouldEqual expectedFs

[<Test>]
let ``Undo move item handles move error by returning error``() =
    let fs = FakeFileSystem [
        folder "dest" [
            file "file"
        ]
        folder "other" []
    ]
    let moved = fs.Item "/c/dest/file"
    let original = createFile "/c/file"
    fs.AddExn true ex "/c/file"
    let intent, putItem = createPutIntentAndItem original moved.Path
    let action = PutItems (Move, intent, [putItem], false)
    let model = testModel |> withLocation "/c/other" |> pushUndo action
    let expectedFs = fs.Items

    let actual = seqResult (Action.undo fs progress) model

    let expectedError = MainStatus.PutError (true, Move, [(moved.Path, ex)], 1)
    let expected = model |> MainModel.withError expectedError |> popUndo |> withNewCancelToken
    assertAreEqual expected actual
    fs.Items |> shouldEqual expectedFs

// redo move tests

[<Test>]
let ``Redo move folder that was an overwrite merges correctly``() =
    let fs = FakeFileSystem [
        folder "dest" [
            folder "another" []
            folder "moved" [
                fileWith (size 1L) "file"
            ]
        ]
        folder "moved" [
            fileWith (size 7L) "file"
            file "other"
        ]
    ]
    let src = createFolder "/c/moved"
    let destPath = createPath "/c/dest/moved"
    let intent = { createPutIntent [src] destPath.Parent with Overwrite = true }
    let createPutItem = createPutItemFrom src.Path destPath
    let actualMoved = [
        createFolder "/c/moved" |> createPutItem |> withDestExists
        createFile "/c/moved/file" |> size 2L |> createPutItem
        createFile "/c/moved/other" |> createPutItem
    ]
    let action = PutItems (Move, intent, actualMoved, false)
    let model = testModel |> pushRedo action

    let actual = seqResult (Action.redo fs progress) model

    let expectedItems = [
        createFolder "/c/dest/another"
        createFolder "/c/dest/moved"
    ]
    let expectedActual = [
        actualMoved.[0]
        // even though this file was not originally an overwrite, a redo should overwrite since we're redoing the intent
        // to merge the folder
        actualMoved.[1] |> withDestExists
        actualMoved.[2]
    ]
    let expectedAction = PutItems (Move, intent, expectedActual, false)
    let expected =
        { (model |> MainModel.withPushedLocation destPath.Parent) with
            Directory = expectedItems
            Items = expectedItems
            Cursor = 1
            UndoStack = expectedAction :: model.UndoStack
            RedoStack = model.RedoStack.Tail
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (MainStatus.RedoAction (expectedAction, model.PathFormat, 1, 1))
        |> withLocationOnHistory
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        folder "dest" [
            folder "another" []
            folder "moved" [
                fileWith (size 7L) "file"
                file "other"
            ]
        ]
    ]

// copy tests

[<TestCase(0)>]
[<TestCase(1)>]
[<TestCase(2)>]
let ``Put file to copy in same folder calls file sys copy with new name`` existingCopies =
    let fs = FakeFileSystem [
        file "file"
        yield! getCopyNames "file" existingCopies |> List.map file
    ]
    let src = fs.Item "/c/file"
    let model = testModel |> withReg (Some (Copy, [src.Ref]))

    let actual = seqResult (Action.put fs progress false) model

    let expectedItems =
        [
            "file"
            yield! getCopyNames "file" (existingCopies+1)
        ]
        |> List.map (fun name -> createFile ("/c/" + name))
        |> sortByPath
    let expectedPath = createPath ("/c/" + getCopyName "file" existingCopies)
    let intent, putItem = createPutIntentAndItem (createFile "/c/file") expectedPath
    let expectedAction = PutItems (Copy, intent, [putItem], false)
    let expected =
        { testModel with
            Directory = expectedItems
            Items = expectedItems
            Cursor = expectedItems |> List.findIndex (fun i -> i.Path = expectedPath)
            UndoStack = expectedAction :: testModel.UndoStack
            RedoStack = []
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (MainStatus.ActionComplete (expectedAction, model.PathFormat))
        |> withLocationOnHistory
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        file "file"
        yield! getCopyNames "file" (existingCopies+1) |> List.map file
    ]

// redo copy tests

[<Test>]
let ``Redo copy item to same parent where copy already exists copies to next name`` () =
    let fs = FakeFileSystem [
        folder "folder" [
            file "another"
            file "file"
            file (getCopyName "file" 0)
        ]
    ]
    let src = fs.Item "/c/folder/file"
    let dest = createPath ("/c/folder/" + getCopyName "file" 0)
    let expectedDest = createPath ("/c/folder/" + getCopyName "file" 1)
    let intent, putItem = createPutIntentAndItem src dest
    let action = PutItems (Copy, intent, [putItem], false)
    let model = testModel |> pushRedo action

    let actual = seqResult (Action.redo fs progress) model

    let expectedItems =
        [
            "another"
            "file"
            getCopyName "file" 0
            getCopyName "file" 1
        ]
        |> List.map (fun name -> createFile ("/c/folder/" + name))
        |> sortByPath
    let expectedAction = PutItems (Copy, intent, [createPutItem src expectedDest], false)
    let expected =
        { (model |> MainModel.withPushedLocation dest.Parent) with
            Directory = expectedItems
            Items = expectedItems
            Cursor = expectedItems.Length - 1
            UndoStack = expectedAction :: testModel.UndoStack
            RedoStack = model.RedoStack.Tail
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (MainStatus.RedoAction (expectedAction, model.PathFormat, 1, 1))
        |> withLocationOnHistory
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        folder "folder" [
            file "another"
            file "file"
            file (getCopyName "file" 0)
            file (getCopyName "file" 1)
        ]
    ]

[<Test>]
let ``Redo copy folder to same parent that was cancelled resumes copy`` () =
    let otherCopyName = Action.getCopyName "fruit" 0
    let copyName = Action.getCopyName "fruit" 1
    let fs = FakeFileSystem [
        folder "fruit" [
            folder "amazing" [
                fileWith (size 2L) "banana"
            ]
            file "apple"
            file "cherry"
            file "dewberry"
        ]
        folder otherCopyName [] // to make sure redo doesn't try to re-determine copy name but resume using the same one
        folder copyName [
            folder "amazing" [
                fileWith (size 7L) "banana" // simulate copied file changing, which should not affect resuming
            ]
            file "apple"
        ]
    ]
    let src = fs.Item "/c/fruit"
    let dest = createFolder ("/c/" + copyName)
    let intent = createPutIntent [src] dest.Path.Parent
    let createPutItem = createPutItemFrom src.Path dest.Path
    let undoActualPut = List.map createPutItem [
        createFolder "/c/fruit"
        createFolder "/c/fruit/amazing"
        createFile "/c/fruit/amazing/banana" |> size 2L
        createFile "/c/fruit/apple"
    ]
    let redoActualPut = List.map createPutItem [
        createFile "/c/fruit/cherry"
        createFile "/c/fruit/dewberry"
    ]
    let model =
        testModel
        |> pushUndo (PutItems (Copy, intent, undoActualPut, true))
        |> pushRedo (PutItems (Copy, intent, redoActualPut, true))

    let actual = seqResult (Action.redo fs progress) model

    let expectedStatusAction = PutItems (Copy, intent, redoActualPut, false)
    let expectedMergedAction = PutItems (Copy, intent, undoActualPut @ redoActualPut, false)
    let expectedItems = [
        src
        createFolder ("/c/" + otherCopyName)
        dest
    ]
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            Cursor = 2
            UndoStack = expectedMergedAction :: testModel.UndoStack
            RedoStack = testModel.RedoStack
            CancelToken = CancelToken()
        }
        |> withReg None
        |> MainModel.withMessage (MainStatus.RedoAction (expectedStatusAction, model.PathFormat, 1, 1))
        |> withLocationOnHistory
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        folder "fruit" [
            folder "amazing" [
                fileWith (size 2L) "banana"
            ]
            file "apple"
            file "cherry"
            file "dewberry"
        ]
        folder otherCopyName []
        folder copyName [
            folder "amazing" [
                fileWith (size 7L) "banana"
            ]
            file "apple"
            file "cherry"
            file "dewberry"
        ]
    ]

// undo copy tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo copy file deletes it`` curPathDifferent =
    let fs = FakeFileSystem [
        drive 'c' [
            folder "other" []
            folder "src" [
                file "file"
            ]
            file "file"
        ]
    ]
    let original = fs.Item "/c/src/file"
    let copied = fs.Item "/c/file"
    let intent, putItem = createPutIntentAndItem original copied.Path
    let action = PutItems (Copy, intent, [putItem], false)
    let location = if curPathDifferent then "/c/other" else "/c"
    let model = testModel |> withLocation location |> pushUndo action

    let actual = seqResult (Action.undo fs progress) model

    let expectedItems = [
        createFolder "/c/other"
        createFolder "/c/src"
    ]
    let expected =
        { model with
            Directory = if curPathDifferent then model.Directory else expectedItems
            Items = if curPathDifferent then model.Items else expectedItems
            UndoStack = model.UndoStack.Tail
            RedoStack = action :: model.RedoStack
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (MainStatus.UndoAction (action, model.PathFormat, 1, 1))
        |> if not curPathDifferent then withLocationOnHistory else id
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        drive 'c' [
            folder "other" []
            folder "src" [
                file "file"
            ]
        ]
    ]
    fs.RecycleBin |> shouldEqual []

[<Test>]
let ``Undo copy empty folder deletes it`` () =
    let fs = FakeFileSystem [
        drive 'c' [
            folder "dest" [
                folder "folder" []
            ]
            folder "folder" []
        ]
    ]
    let original = fs.Item "/c/folder"
    let copied = fs.Item "/c/dest/folder"
    let intent, putItem = createPutIntentAndItem original copied.Path
    let action = PutItems (Copy, intent, [putItem], false)
    let model = testModel |> pushUndo action

    let actual = seqResult (Action.undo fs progress) model

    let expected =
        { model with
            UndoStack = model.UndoStack.Tail
            RedoStack = action :: model.RedoStack
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (MainStatus.UndoAction (action, model.PathFormat, 1, 1))
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        drive 'c' [
            folder "dest" []
            folder "folder" []
        ]
    ]
    fs.RecycleBin |> shouldEqual []

[<Test>]
let ``Undo copy empty folder that has new items in it returns error`` () =
    let fs = FakeFileSystem [
        drive 'c' [
            folder "dest" [
                folder "folder" [
                    file "new" // simulate externally-created file after folder copy
                ]
            ]
            folder "folder" []
        ]
    ]
    let original = fs.Item "/c/folder"
    let copied = fs.Item "/c/dest/folder"
    let intent, putItem = createPutIntentAndItem original copied.Path
    let action = PutItems (Copy, intent, [putItem], false)
    let model = testModel |> pushUndo action

    let actual = seqResult (Action.undo fs progress) model

    let expectedExn = FakeFileSystemErrors.cannotDeleteNonEmptyFolder
    let expected =
        { model with
            UndoStack = model.UndoStack.Tail
            CancelToken = CancelToken()
        }
        |> MainModel.withError (MainStatus.PutError (true, Copy, [copied.Path, expectedExn], 1))
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        drive 'c' [
            folder "dest" [
                folder "folder" [
                    file "new"
                ]
            ]
            folder "folder" []
        ]
    ]
    fs.RecycleBin |> shouldEqual []

[<TestCase(false, false, false)>]
[<TestCase(false, false, true)>]
[<TestCase(false, true, false)>]
[<TestCase(false, true, true)>]
[<TestCase(true, true, false)>]
[<TestCase(true, true, true)>]
let ``Undo copy folder deletes items that were copied and removes dest folders if empty`` sameParent isLocationDest hasNewItem =
    let original = createFolder "/c/folder"
    let copied = createFolder (if sameParent then "/c/" + getCopyName "folder" 0 else "/c/dest/folder")
    let copiedTree =
        folder copied.Name [
            folder "sub" [
                file "inner"
            ]
            file "file"
            if hasNewItem then
                file "new"
            file "other"
        ]
    let fs = FakeFileSystem [
        folder "folder" [
            folder "sub" [
                file "inner"
            ]
            file "file"
            file "other"
        ]
        if sameParent then
            copiedTree
        else
            folder "dest" [
                folder "another" []
                copiedTree
            ]
    ]
    let intent = createPutIntent [original] copied.Path.Parent
    let actualCopied = List.map (createPutItemFrom original.Path copied.Path) [
        createFolder "/c/folder"
        createFolder "/c/folder/sub"
        createFile "/c/folder/sub/inner"
        createFile "/c/folder/file"
        createFile "/c/folder/other"
    ]
    let action = PutItems (Copy, intent, actualCopied, false)
    let model =
        testModel
        |> pushUndo action
        |> (if isLocationDest then MainModel.withLocation copied.Path.Parent else id)
        |> withHistoryPaths (historyPaths {
            actualCopied.[0].Dest, true
            actualCopied.[1].Dest, true
            actualCopied.[2].Dest, false
            actualCopied.[3].Dest, false
            if hasNewItem then
                copied.Path.Join "new", false
        })

    let actual = seqResult (Action.undo fs progress) model

    let expectedItems =
        if isLocationDest then
            [
                if sameParent then
                    createFolder "/c/folder"
                else
                    createFolder "/c/dest/another"
                if hasNewItem then
                    copied
            ]
        else
            testModel.Items
    let expectedRedoAction = PutItems (Copy, intent, actualCopied |> applyIf hasNewItem (List.skip 1), false)
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            CancelToken = CancelToken()
        }
        |> MainModel.withStatus (
            if hasNewItem then
                let expectedErrorPaths = [copied.Path, FakeFileSystemErrors.cannotDeleteNonEmptyFolder]
                MainStatus.Error (MainStatus.PutError (true, Copy, expectedErrorPaths, actualCopied.Length))
            else
                MainStatus.Message (MainStatus.UndoAction (action, model.PathFormat, 1, 1))
        )
        |> popUndo
        |> pushRedo expectedRedoAction
        |> withHistoryPaths (historyPaths {
            if isLocationDest then
                model.Location, true
            if hasNewItem then
                copied
                copied.Path.Join "new", false
        })
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        folder "folder" [
            folder "sub" [
                file "inner"
            ]
            file "file"
            file "other"
        ]
        if sameParent then
            if hasNewItem then
                folder copied.Name [
                    file "new"
                ]
        else
            folder "dest" [
                folder "another" []
                if hasNewItem then
                    folder "folder" [
                        file "new"
                    ]
            ]
    ]
    fs.RecycleBin |> shouldEqual []

[<Test>]
let ``Undo copy does nothing for items that were overwrites`` () =
    let fs = FakeFileSystem [
        driveWithSize 'c' 100L [
            folder "copied" [
                file "file"
                file "other"
            ]
            folder "dest" [
                folder "copied" [
                    file "file"
                    file "other"
                ]
            ]
        ]
    ]
    let copied = fs.Item "/c/dest/copied"
    let original = createFolder "/c/copied"
    let intent = { createPutIntent [original] copied.Path.Parent with Overwrite = true }
    let createPutItem = createPutItemFrom original.Path copied.Path
    let actualCopied = [
        createFile "/c/copied/file" |> createPutItem |> withDestExists
        createFile "/c/copied/other" |> createPutItem
    ]
    let action = PutItems (Copy, intent, actualCopied, false)
    let model =
        testModel
        |> pushUndo action
        |> withHistoryPaths (historyPaths {
            copied
            actualCopied.[0].Dest, false
            actualCopied.[1].Dest, false
        })

    let actual = seqResult (Action.undo fs progress) model

    let expectedAction = PutItems (Copy, intent, actualCopied |> List.filter (fun pi -> not pi.DestExists), false)
    let expected =
        { model with
            UndoStack = model.UndoStack.Tail
            RedoStack = expectedAction :: testModel.RedoStack
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (MainStatus.UndoAction (expectedAction, testModel.PathFormat, 1, 1))
        |> withHistoryPaths model.History.Paths.[0..1]
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        driveWithSize 'c' 100L [
            folder "copied" [
                file "file"
                file "other"
            ]
            folder "dest" [
                folder "copied" [
                    file "file"
                ]
            ]
        ]
    ]

type UndoCopyError =
    | DeleteFileError
    | DeleteFolderError
    | DeleteFolderNotEmpty

let undoCopyErrorCases = [
    TestCaseData(DeleteFileError, false)
    TestCaseData(DeleteFileError, true)
    TestCaseData(DeleteFolderError, false)
    TestCaseData(DeleteFolderNotEmpty, false)
]

[<TestCaseSource(nameof undoCopyErrorCases)>]
let ``Undo copy handles partial success by updating redo and setting error message`` undoCopyError hasCancelledRedoItem =
    let isFolderError =
        match undoCopyError with
        | DeleteFolderError | DeleteFolderNotEmpty -> true
        | _ -> false
    let hasNewItem =
        match undoCopyError with
        | DeleteFolderNotEmpty -> true
        | _ -> false
    let fs = FakeFileSystem [
        driveWithSize 'c' 100L [
            folder "copied" [
                file "file"
                file "other"
            ]
            folder "dest" [
                folder "copied" [
                    file "file"
                    if hasNewItem then
                        file "new"
                    file "other"
                ]
            ]
        ]
    ]
    let copied = fs.Item "/c/dest/copied"
    let original = createFolder "/c/copied"
    let errorItem =
        if isFolderError
        then copied
        else fs.Item "/c/dest/copied/file"
    if not hasNewItem then
        fs.AddExnPath true ex errorItem.Path
    let intent = createPutIntent [original] copied.Path.Parent
    let actualCopied = List.map (createPutItemFrom original.Path copied.Path) [
        createFolder "/c/copied"
        createFile "/c/copied/file"
        createFile "/c/copied/other"
    ]
    let action = PutItems (Copy, intent, actualCopied, false)
    let prevCopyItem = createFile "/c/copied/prev" |> createPutItemFrom original.Path copied.Path
    let model =
        testModel
        |> pushUndo action
        |> if hasCancelledRedoItem
            then pushRedo (PutItems (Copy, intent, [prevCopyItem], true))
            else id
        |> withHistoryPaths (historyPaths {
            actualCopied.[0].Dest, true
            actualCopied.[1].Dest, false
            actualCopied.[2].Dest, false
        })

    let actual = seqResult (Action.undo fs progress) model

    let expectedActual = [
        if isFolderError then
            actualCopied.[1]
        actualCopied.[2]
        if hasCancelledRedoItem then
            prevCopyItem
    ]
    let expectedAction = PutItems (Copy, intent, expectedActual, false)
    let nonEmptyEx = FakeFileSystemErrors.cannotDeleteNonEmptyFolder
    let expectedErrorPaths = [
        (errorItem.Path, if hasNewItem then nonEmptyEx else ex)
        if not isFolderError then
            (copied.Path, nonEmptyEx)
    ]
    let expected =
        { model with
            UndoStack = model.UndoStack.Tail
            RedoStack = expectedAction :: testModel.RedoStack
            CancelToken = CancelToken()
        }
        |> MainModel.withError (MainStatus.PutError (true, Copy, expectedErrorPaths, actualCopied.Length))
        |> withHistoryPaths [
            model.History.Paths.[0]
            if not isFolderError then
                model.History.Paths.[1]
        ]
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        driveWithSize 'c' 100L [
            folder "copied" [
                file "file"
                file "other"
            ]
            folder "dest" [
                folder "copied" [
                    if not isFolderError then
                        file "file"
                    if hasNewItem then
                        file "new"
                ]
            ]
        ]
    ]

[<Test>]
let ``Undo copy enumerated folder handles partial success with cancellation by updating undo and redo and setting error message`` () =
    let itemTree =
        folder "fruit" [
            folder "amazing" [
                file "apple"
                file "banana"
            ]
            folder "bad" [
                file "grapefruit"
            ]
            file "cherry"
        ]
    let fs = FakeFileSystem [
        drive 'c' [
            itemTree
        ]
        drive 'd' [
            itemTree
            file "other"
        ]
    ]
    let original = createFolder "/c/fruit"
    let dest = fs.Item "/d/fruit"
    let intent = createPutIntent [original] dest.Path.Parent
    let actualPut = List.map (createPutItemFrom original.Path dest.Path) [
        createFolder "/c/fruit"
        createFolder "/c/fruit/amazing"
        createFile "/c/fruit/amazing/apple"
        createFile "/c/fruit/amazing/banana"
        createFolder "/c/fruit/bad"
        createFile "/c/fruit/bad/grapefruit"
        createFile "/c/fruit/cherry"
    ]
    let action = PutItems (Copy, intent, actualPut, false)
    let errorItem = actualPut.[5] // bad/grapefruit
    fs.AddExnPath true ex errorItem.Dest
    let model =
        testModel
        |> withLocation "/d"
        |> pushUndo action
        |> withHistoryPaths (historyPaths {
            actualPut.[0].Dest, true
            actualPut.[1].Dest, true
            actualPut.[2].Dest, false
            actualPut.[3].Dest, false
            actualPut.[4].Dest, true
            actualPut.[5].Dest, false
            actualPut.[6].Dest, false
        })
    let writesBeforeCancel = 4

    let actual = seqResultWithCancelTokenCallback (fs.CancelAfterWriteCount writesBeforeCancel) (Action.undo fs progress) model

    let expectedUnsuccessful = [
        actualPut.[0]
        actualPut.[1]
        actualPut.[2]
        actualPut.[4]
        actualPut.[5]
    ]
    let expectedErrors = [
        (actualPut.[5].Dest, ex)
        (actualPut.[4].Dest, FakeFileSystemErrors.cannotDeleteNonEmptyFolder)
    ]
    let expectedUndoAction = PutItems (Copy, intent, expectedUnsuccessful, true)
    let expectedRedoAction = PutItems (Copy, intent, actualPut |> List.except expectedUnsuccessful, true)
    let expectedItems = [
            dest
            createFile "/d/other"
        ]
    let expected =
        { model with
            Directory = expectedItems |> sortByPath
            Items = expectedItems
            Cursor = 0
            UndoStack = expectedUndoAction :: testModel.UndoStack
            RedoStack = expectedRedoAction :: testModel.RedoStack
        }
        |> MainModel.withError (MainStatus.PutError (true, Copy, expectedErrors, actualPut.Length))
        |> withHistoryPaths (historyPaths {
            actualPut.[0].Dest, true
            actualPut.[1].Dest, true
            actualPut.[2].Dest, false
            actualPut.[4].Dest, true
            actualPut.[5].Dest, false
        })
        |> withLocationOnHistory
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        drive 'c' [
            itemTree
        ]
        drive 'd' [
            folder "fruit" [
                folder "amazing" [
                    file "apple"
                ]
                folder "bad" [
                    file "grapefruit"
                ]
            ]
            file "other"
        ]
    ]

[<Test>]
let ``Undo copy item handles errors by returning error and consuming action`` () =
    let fs = FakeFileSystem [
        driveWithSize 'c' 100L [
            folder "src" [
                file "file"
            ]
            file "file"
        ]
    ]
    let original = fs.Item "/c/src/file"
    let copied = fs.Item "/c/file"
    let intent, putItem = createPutIntentAndItem original copied.Path
    fs.AddExnPath false ex copied.Path
    fs.AddExnPath true (exn "Write error") copied.Path
    let action = PutItems (Copy, intent, [putItem], false)
    let model = testModel |> pushUndo action
    let expectedFs = fs.Items

    let actual = seqResult (Action.undo fs progress) model

    let expectedError = MainStatus.PutError (true, Copy, [copied.Path, ex], 1)
    let expected = model |> MainModel.withError expectedError |> popUndo |> withNewCancelToken
    assertAreEqual expected actual
    fs.Items |> shouldEqual expectedFs
    fs.RecycleBin |> shouldEqual []

// shortcut tests

[<TestCase(false, false)>]
[<TestCase(false, true)>]
[<TestCase(true, false)>]
[<TestCase(true, true)>]
let ``Put shortcut calls file sys create shortcut`` isFolder overwrite =
    let fs = FakeFileSystem [
        folder "src" [
            if isFolder then
                folder "item" []
            else
                file "item"
        ]
        if overwrite then
            file "item.lnk"
    ]
    let target = fs.Item "/c/src/item"
    let shortcut = createFile "/c/item.lnk"
    let model = testModel |> withReg (Some (Shortcut, [target.Ref]))

    let actual = seqResult (Action.put fs progress overwrite) model

    let expectedIntent = { createPutIntent [target] shortcut.Path.Parent with Overwrite = overwrite }
    let expectedPutItem = { createPutItem target shortcut.Path with DestExists = overwrite }
    let expectedAction = PutItems (Shortcut, expectedIntent, [expectedPutItem], false)
    let expectedItems = [
        createFolder "/c/src"
        shortcut
    ]
    let expected =
        { testModel with
            Directory = expectedItems
            Items = expectedItems
            Cursor = 1
            UndoStack = expectedAction :: testModel.UndoStack
            RedoStack = []
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (MainStatus.ActionComplete (expectedAction, model.PathFormat))
        |> withLocationOnHistory
    assertAreEqual expected actual
    fs.GetShortcutTarget shortcut.Path |> shouldEqual (Ok (string target.Path))

// undo shortcut tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo create shortcut deletes shortcut`` curPathDifferent =
    let fs = FakeFileSystem [
        folder "folder" [
            file "file"
        ]
        file "file.lnk"
    ]
    let target = fs.Item "/c/folder/file"
    let shortcut = fs.Item "/c/file.lnk"
    let intent, putItem = createPutIntentAndItem target shortcut.Path
    let action = PutItems (Shortcut, intent, [putItem], false)
    let location = if curPathDifferent then "/c/other" else "/c"
    let model =
        testModel
        |> withLocation location
        |> pushUndo action
        |> withHistoryPaths [itemHistoryPath shortcut]

    let actual = seqResult (Action.undo fs progress) model

    let expectedItems = [
        createFolder "/c/folder"
    ]
    let expected =
        model
        |> MainModel.withMessage (MainStatus.UndoAction (action, model.PathFormat, 1, 1))
        |> popUndo
        |> pushRedo action
        |> withHistoryPaths []
        |> fun model ->
            if curPathDifferent then
                model
            else
                { model with
                    Directory = expectedItems
                    Items = expectedItems
                    Cursor = 0
                }
                |> withLocationOnHistory
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        folder "folder" [
            file "file"
        ]
    ]
    fs.RecycleBin |> shouldEqual []

[<Test>]
let ``Undo create shortcut handles errors by returning error and consuming action`` () =
    let fs = FakeFileSystem [
        file "file.lnk"
    ]
    let target = createFile "/c/folder/file"
    let shortcut = fs.Item "/c/file.lnk"
    fs.AddExnPath false ex shortcut.Path
    let intent, putItem = createPutIntentAndItem target shortcut.Path
    let action = PutItems (Shortcut, intent, [putItem], false)
    let model =
        testModel
        |> pushUndo action
        |> withHistoryPaths [itemHistoryPath shortcut]

    let actual = seqResult (Action.undo fs progress) model

    let statusAction = DeletedItems (true, [shortcut], false)
    let expected =
        model
        |> popUndo
        |> MainModel.withError (MainStatus.ItemActionError (statusAction, model.PathFormat, ex))
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        file "file.lnk"
    ]

// undo/redo multiple action tests

[<Test>]
let ``Undo multiple actions using repeat count does each action and updates status each time`` () =
    let fs = FakeFileSystem [
        folder "folder" [
            file "renamed"
        ]
        file "created"
    ]
    let intent, putItem = createPutIntentAndItem (createFile "/c/moved") (createPath "/c/folder/moved")
    let actions = [
        PutItems (Move, intent, [putItem], false)
        CreatedItem (createFile "/c/created")
        RenamedItem (createFile "/c/folder/moved", "renamed")
    ]
    let model =
        { testModel with UndoStack = (actions |> List.rev) @ testModel.UndoStack }
        |> withRepeat 3

    let actual = seqResult (Action.undo fs progress) model

    let undoStatus iterFromLatest action =
        let iter = actions.Length - iterFromLatest
        MainStatus.Message (MainStatus.UndoAction (action, model.PathFormat, iter, actions.Length))
    let expectedItems = [
        createFolder "/c/folder"
        createFile "/c/moved"
    ]
    let expectedBack = [
        (createPath "/c/folder", 0)
        (createPath "/c", testModel.Cursor)
    ]
    let expected =
        { testModel with
            Directory = expectedItems
            Items = expectedItems
            Cursor = 1
            BackStack = expectedBack @ testModel.BackStack
            ForwardStack = []
            RedoStack = actions @ testModel.RedoStack
            RepeatCommand = model.RepeatCommand // this is cleared later by MainLogic.keyPress
            CancelToken = CancelToken()
        }
        |> MainModel.withStatus (actions |> List.head |> undoStatus 0)
        |> withHistoryPaths [
            createHistoryPath "/c/"
            createHistoryPath "/c/folder/"
        ]
    assertAreEqual expected actual
    actual.StatusHistory |> shouldEqual (actions |> List.mapi undoStatus)
    fs.ItemsShouldEqual [
        folder "folder" []
        file "moved"
    ]

[<Test>]
let ``Redo multiple actions using repeat count does each action and updates status each time`` () =
    let fs = FakeFileSystem [
        folder "folder" []
        file "moved"
    ]
    let intent, putItem = createPutIntentAndItem (createFile "/c/moved") (createPath "/c/folder/moved")
    let actions = [
        PutItems (Move, intent, [putItem], false)
        CreatedItem (createFile "/c/created")
        RenamedItem (createFile "/c/folder/moved", "renamed")
    ]
    let model =
        { testModel with RedoStack = actions @ testModel.RedoStack }
        |> withRepeat 3

    let actual = seqResult (Action.redo fs progress) model

    let redoStatus iterFromLatest action =
        let iter = actions.Length - iterFromLatest
        MainStatus.Message (MainStatus.RedoAction (action, model.PathFormat, iter, actions.Length))
    let expectedItems = [
        createFile "/c/folder/renamed"
    ]
    let expectedBack = [
        (createPath "/c", 1)
        (createPath "/c/folder", 0)
        (createPath "/c", testModel.Cursor)
    ]
    let expected =
        { testModel with
            Location = createPath "/c/folder"
            LocationInput = "/c/folder/"
            Cursor = 0
            Directory = expectedItems
            Items = expectedItems
            BackStack = expectedBack @ testModel.BackStack
            ForwardStack = []
            UndoStack = (actions |> List.rev) @ testModel.UndoStack
            RepeatCommand = model.RepeatCommand // this is cleared later by MainLogic.keyPress
            CancelToken = CancelToken()
        }
        |> MainModel.withStatus (actions |> List.last |> redoStatus 0)
        |> withHistoryPaths [
            createHistoryPath "/c/folder/"
            createHistoryPath "/c/"
        ]
    assertAreEqual expected actual
    actual.StatusHistory |> shouldEqual (actions |> List.rev |> List.mapi redoStatus)
    fs.ItemsShouldEqual [
        folder "folder" [
            file "renamed"
        ]
        file "created"
    ]
