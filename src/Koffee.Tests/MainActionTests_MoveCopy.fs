module Koffee.MainActionTests_MoveCopy

open System
open NUnit.Framework
open FsUnitTyped
open Acadian.FSharp
open Koffee.Main

let withReg reg model =
    { model with Config = { model.Config with YankRegister = reg } }

let copyName = Action.getCopyName

let copyNames name num =
    List.init num (copyName name)

let putTypeCases () = [
    TestCaseData(Move)
    TestCaseData(Copy)
    TestCaseData(Shortcut)
]

let putItemOverwriteCases () =
    putTypeCases () |> List.collect (fun c -> [
        TestCaseData(c.Arguments.[0], false)
        TestCaseData(c.Arguments.[0], true)
    ])

let assertAreEqual expected actual =
    assertAreEqualWith expected actual (fun comp -> comp.Config.MembersToIgnore.Remove "MainModel.History" |> ignore)

[<TestCaseSource("putItemOverwriteCases")>]
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
    let item = Some (src.Path, src.Type, putType)
    let model = testModel |> withReg item
    let expectedItems = fs.ItemsIn "/c"

    let actual = seqResult (Action.put fs progress false) model

    let expected =
        { model with
            Directory = expectedItems |> sortByPath
            Items = expectedItems |> List.filter (fun i -> i.Name <> "hidden")
            Cursor = 2
            InputMode = Some (Confirm (Overwrite (putType, src, dest)))
        }
        |> withLocationOnHistory
    assertAreEqual expected actual
    fs.ItemsIn "/c" |> shouldEqual expectedItems

[<Test>]
let ``Put handles missing register item`` () =
    let src = createFile "/c/file"
    let fs = FakeFileSystem []
    let model = testModel |> withReg (Some (src.Path, src.Type, Move))

    let actual = seqResult (Action.put fs progress false) model

    let expected = model.WithError (MainStatus.YankRegisterItemMissing (src.Path.Format model.PathFormat))
    assertAreEqual expected actual

[<Test>]
let ``Put handles error reading register item`` () =
    let src = createFile "/c/file"
    let fs = FakeFileSystem []
    fs.AddExnPath false ex src.Path
    let model = testModel |> withReg (Some (src.Path, src.Type, Move))

    let actual = seqResult (Action.put fs progress false) model

    let expected = model.WithError (MainStatus.ActionError ("read yank register item", ex))
    assertAreEqual expected actual

[<TestCaseSource("putTypeCases")>]
let ``Put item handles file system errors`` putType =
    let fs = FakeFileSystem [
        folder "folder" [
            file "file"
        ]
    ]
    let src = fs.Item "/c/folder/file"
    let destPath = "/c/file" + (if putType = Shortcut then ".lnk" else "") |> createPath
    fs.AddExnPath true ex destPath
    let item = Some (src.Path, src.Type, putType)
    let model = testModel |> withReg item
    let expectedFs = fs.Items

    let actual = seqResult (Action.put fs progress false) model

    let expected = model.WithError (MainStatus.PutError (false, putType, [(src, ex)], 1)) |> withNewCancelToken
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
        |> withReg (Some (src.Path, src.Type, putType))
        |> withHistoryPaths [itemHistoryPath src]

    let actual = seqResult (Action.put fs progress overwrite) model

    let putItem = { Item = src; Dest = dest.Path; DestExists = overwrite }
    let expectedAction = PutItems (putType, putItem, [putItem], false)
    let expectedItems = [
        createFolder "/c/folder"
        createFile "/c/file" |> size 41L
    ]
    let expected =
        { testModel with
            Directory = expectedItems |> sortByPath
            Items = expectedItems
            Cursor = 1
            UndoStack = expectedAction :: testModel.UndoStack
            RedoStack = []
            CancelToken = CancelToken()
        }.WithMessage (MainStatus.ActionComplete (expectedAction, testModel.PathFormat))
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
    let putItem = { Item = src; Dest = dest; DestExists = false }
    let model =
        testModel.WithLocation dest.Parent
        |> if isRedo
            then pushRedo (PutItems (putType, putItem, [], false))
            else withReg (Some (src.Path, src.Type, putType))
        |> withHistoryPaths (historyPaths {
            "/c/fruit/amazing/banana"
            "/d/other/"
            errorItem
            src
        })
    let testFunc =
        if isRedo
        then Action.redo fs progress
        else Action.put fs progress true

    let actual = seqResult testFunc model

    let expectedPut = List.map (createPutItem src.Path dest) [
        createFile "/c/fruit/amazing/banana"
        createFile "/c/fruit/apple"
        createFile "/c/fruit/cherry"
    ]
    let expectedAction = PutItems (putType, putItem, expectedPut, false)
    let expectedItems = [
        createFolder "/d/fruit"
        createFolder "/d/other"
    ]
    let expectedError = MainStatus.PutError (false, putType, [(errorItem, ex)], 4)
    let expected =
        { testModel with
            Directory = expectedItems |> sortByPath
            Items = expectedItems
            Cursor = 0
            UndoStack = expectedAction :: testModel.UndoStack
            RedoStack = if isRedo then testModel.RedoStack else []
            CancelToken = CancelToken()
        }.WithLocation(dest.Parent).WithError(expectedError)
        |> withHistoryPaths (historyPaths {
            if copy then
                expectedPut.[0].Item
            else
                expectedPut.[0].Dest, false
            yield! model.History.Paths.[1..3]
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
    let dest = createPath "/d/fruit"
    let putType = if copy then Copy else Move
    let putItem = { Item = src; Dest = dest; DestExists = false }
    let actualPut = List.map (createPutItem src.Path dest) [
        createFile "/c/fruit/amazing/banana"
        createFile "/c/fruit/apple"
        createFile "/c/fruit/cherry"
        createFile "/c/fruit/dewberry" |> size 7L
    ]
    let regItem =
        if not isRedo
        then Some (src.Path, src.Type, putType)
        else None
    let model =
        testModel
        |> withLocation "/d"
        |> withReg regItem
        |> if isRedo
            then pushRedo (PutItems (putType, putItem, actualPut, false))
            else id
        |> withHistoryPaths (historyPaths {
            "/d/"
            src
            actualPut.[1].Item
            actualPut.[2].Item
            actualPut.[3].Item
            actualPut.[1].Dest, false
        })

    let testFunc isResume =
        if isRedo
        then Action.redo fs progress
        else Action.put fs progress isResume

    // part one: put or redo cancels correctly
    let modelAfterCancel = seqResultWithCancelTokenCallback (fs.CancelAfterWriteCount 2) (testFunc false) model

    (
        let expectedAction = PutItems (putType, putItem, actualPut |> List.take 2, true)
        let expectedRedo =
            PutItems (putType, putItem, actualPut |> List.skip 2, true)
            :: (if isRedo then testModel.RedoStack else [])
        let expectedItems = [createFolder "/d/fruit"]
        let expected =
            { model with
                Directory = expectedItems |> sortByPath
                Items = expectedItems
                Cursor = 0
                UndoStack = expectedAction :: testModel.UndoStack
                RedoStack = expectedRedo
            }.WithMessage (MainStatus.CancelledPut (putType, false, 2, 4))
            |> withReg None
            |> withHistoryPaths (
                if copy then
                    model.History.Paths
                else
                    historyPaths {
                        "/d/"
                        src
                        actualPut.[1].Dest, false
                        actualPut.[2].Item
                        actualPut.[3].Item
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
    let putItemWithConflict = actualPut.[3]
    fs.Create File putItemWithConflict.Dest |> assertOk

    // part two: put or redo again completes the operation and merges undo item
    // - put should merge with folder created in part one and overwrite items
    // - redo should return partial success with error on new item since original operation was not an overwrite
    // - redo copy should not copy completed items again
    let actual =
        modelAfterCancel
        |> withReg regItem
        |> seqResult (testFunc true)

    let expectedPut forMergedUndoItem = [
        if not isRedo && copy then
            yield! actualPut.[0..1] |> List.map withDestExists
        else if forMergedUndoItem then
            yield! actualPut.[0..1]
        actualPut.[2]
        if not isRedo then
            actualPut.[3] |> withDestExists
    ]
    let expectedStatusAction = PutItems (putType, putItem |> withDestExists, expectedPut false, false)
    let expectedMergedAction = PutItems (putType, putItem, expectedPut true, false)
    let expectedItems = [createFolder "/d/fruit"]
    let expected =
        { model with
            Directory = expectedItems |> sortByPath
            Items = expectedItems
            Cursor = 0
            UndoStack = expectedMergedAction :: testModel.UndoStack
            RedoStack = if isRedo then testModel.RedoStack else []
            CancelToken = CancelToken()
        }
        |> withReg None
        |> fun model ->
            if isRedo then
                let expectedEx = RedoPutBlockedByExistingItemException() :> exn
                model.WithError (MainStatus.PutError (false, putType, [(putItemWithConflict.Item, expectedEx)], 2))
            else
                model.WithMessage (MainStatus.ActionComplete (expectedStatusAction, testModel.PathFormat))
        |> withHistoryPaths (
            if copy then
                model.History.Paths
            else
                historyPaths {
                    "/d/"
                    if isRedo then
                        src
                    else
                        expectedItems.[0]
                    actualPut.[1].Dest, false
                    actualPut.[2].Dest, false
                    if isRedo then
                        actualPut.[3].Item
                    else
                        actualPut.[3].Dest, false
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
    let putItem = { Item = src; Dest = dest; DestExists = false }
    let actualPut = List.map (createPutItem src.Path dest) [
        createFile "/c/fruit/amazing/banana"
        createFile "/c/fruit/bad/grapefruit"
        createFile "/c/fruit/cherry"
        createFile "/c/fruit/dewberry"
    ]
    let errorItem = actualPut.[1]
    fs.AddExnPath true ex errorItem.Dest
    let regItem =
        if not isRedo
        then Some (src.Path, src.Type, putType)
        else None
    let model =
        testModel
        |> withLocation "/d"
        |> withReg regItem
        |> if isRedo
            then pushRedo (PutItems (putType, putItem, actualPut, false))
            else id
        |> withHistoryPaths (historyPaths {
            "/d/"
            src
            actualPut.[0].Item.Path.Parent, true
            yield! actualPut |> List.map (fun pi -> pi.Item)
        })

    let testFunc =
        if isRedo
        then Action.redo fs progress
        else Action.put fs progress false

    let actual = seqResultWithCancelTokenCallback (fs.CancelAfterWriteCount 2) testFunc model

    let expectedSuccessItems = [
        actualPut.[0]
        actualPut.[2]
    ]
    let expectedUndoAction = PutItems (putType, putItem, expectedSuccessItems, true)
    let expectedRedoAction = PutItems (putType, putItem, actualPut |> List.except expectedSuccessItems, true)
    let expectedItems = [createFolder "/d/fruit"]
    let expected =
        { model with
            Directory = expectedItems |> sortByPath
            Items = expectedItems
            Cursor = 0
            UndoStack = expectedUndoAction :: testModel.UndoStack
            RedoStack = expectedRedoAction :: if isRedo then testModel.RedoStack else []
        }.WithError (MainStatus.PutError (false, putType, [errorItem.Item, ex], actualPut.Length))
        |> withReg None
        |> withHistoryPaths (historyPaths {
            "/d/"
            src
            if copy then
                actualPut.[0].Item.Path.Parent, true
                actualPut.[0].Item
            else
                actualPut.[0].Dest, false
            actualPut.[1].Item
            if copy then
                actualPut.[2].Item
            else
                actualPut.[2].Dest, false
            actualPut.[3].Item
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
                folder "bad" [] // folder should have been created when preparing to move error item
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
    let model = testModel |> withLocation "/d" |> withReg (Some (src.Path, src.Type, putType))
    let expectedFs = fs.Items

    let actual = seqResultWithCancelTokenCallback (fun ct -> ct.Cancel()) (Action.put fs progress false) model

    let expected = model.WithMessage (MainStatus.CancelledPut (putType, false, 0, 0))
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
    let model = testModel |> withLocation "/c/dest" |> withReg (Some (src.Path, src.Type, putType))

    let actual = seqResult (Action.put fs progress true) model

    let intent = { Item = src; Dest = dest.Path; DestExists = true }
    let createPutItem = createPutItem src.Path dest.Path
    let expectedPut = [
        if copy then
            createPutItem (createFile "/c/fruit/bushes/blueberry")
        else
            createPutItem (createFolder "/c/fruit/bushes")
        createPutItem (createFile "/c/fruit/tomato")
        createPutItem (createFile "/c/fruit/trees/apple" |> size 7L) |> withDestExists
        createPutItem (createFile "/c/fruit/trees/banana")
        createPutItem (createFolder "/c/fruit/vines") |> withDestExists
    ]
    let expectedAction = PutItems (putType, intent, expectedPut, false)
    let expectedItems = [dest]
    let expected =
        { testModel with
            Directory = expectedItems |> sortByPath
            Items = expectedItems
            UndoStack = expectedAction :: testModel.UndoStack
            RedoStack = []
            CancelToken = CancelToken()
        }.WithMessage (MainStatus.ActionComplete (expectedAction, testModel.PathFormat))
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
            itemTree
            file "other"
        ]
        drive 'd' [
            if wasCopy then
                itemTree
        ]
    ]
    let dest = fs.Item "/c/fruit"
    let original = createFolder "/d/fruit"
    let putItem = { Item = original; Dest = dest.Path; DestExists = false }
    let actualPut = List.map (createPutItem original.Path dest.Path) [
        createFile "/d/fruit/amazing/banana"
        createFile "/d/fruit/apple"
        createFile "/d/fruit/cherry"
    ]
    let putType = if wasCopy then Copy else Move
    let action = PutItems (putType, putItem, actualPut, false)
    let model =
        testModel
        |> pushUndo action
        |> withHistoryPaths (historyPaths {
            dest
            actualPut.[0].Dest, false
            actualPut.[1].Dest, false
            actualPut.[2].Dest, false
        })

    // part one: undo cancels correctly
    let modelAfterCancel = seqResultWithCancelTokenCallback (fs.CancelAfterWriteCount 2) (Action.undo fs progress) model

    (
        let expectedUndoAction = PutItems (putType, putItem, actualPut |> List.skip 2, true)
        let expectedRedoAction = PutItems (putType, putItem, actualPut |> List.take 2, true)
        let expectedItems =
            if wasCopy then
                [
                    dest
                    createFile "/c/other"
                ]
            else
                [original]
        let expected =
            { model with
                Directory = expectedItems |> sortByPath
                Items = expectedItems
                Cursor = 0
                UndoStack = expectedUndoAction :: testModel.UndoStack
                RedoStack = expectedRedoAction :: testModel.RedoStack
            }.WithMessage (MainStatus.CancelledPut (putType, true, 2, 3))
            |> fun model ->
                if wasCopy
                then model // undo copy does not open a path, only refreshes if current path is destination
                else model.WithPushedLocation original.Path.Parent
            |> withHistoryPaths (historyPaths {
                dest
                if not wasCopy then
                    actualPut.[0].Item
                    actualPut.[1].Item
                actualPut.[2].Dest, false
            })
            |> withLocationOnHistory
        assertAreEqual expected modelAfterCancel
        fs.ItemsShouldEqual [
            drive 'c' [
                folder "fruit" [
                    file "cherry"
                ]
                file "other"
            ]
            drive 'd' [
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
        ]
        printfn "part one: cancellation completed successfully"
    )

    // part two: undo again resumes and completes the operation
    let actual = seqResult (Action.undo fs progress) modelAfterCancel

    let expectedStatusAction = PutItems (putType, putItem, actualPut |> List.skip 2, false)
    let expectedRedoAction = PutItems (putType, putItem, actualPut, false)
    let expectedItems =
        if wasCopy
        then [createFile "/c/other"]
        else [original]
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            Cursor = 0
            UndoStack = model.UndoStack.Tail
            RedoStack = expectedRedoAction :: model.RedoStack
            CancelToken = CancelToken()
        }.WithMessage (MainStatus.UndoAction (expectedStatusAction, model.PathFormat, 1))
        |> fun model ->
            if wasCopy
            then model // undo copy does not open a path, only refreshes if current path is destination
            else model.WithPushedLocation original.Path.Parent
        |> withHistoryPaths (historyPaths {
            if not wasCopy then
                original
                actualPut.[0].Item
                actualPut.[1].Item
                actualPut.[2].Item
        })
        |> withLocationOnHistory
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        drive 'c' [
            file "other"
        ]
        drive 'd' [
            itemTree
        ]
    ]

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo put enumerated folder handles partial success with cancellation by updating undo and redo and setting error message``
        wasCopy =
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
        drive 'c' [
            if wasCopy then
                itemTree
        ]
        drive 'd' [
            itemTree
            file "other"
        ]
    ]
    let original = createFolder "/c/fruit"
    let dest = fs.Item "/d/fruit"
    let putItem = { Item = original; Dest = dest.Path; DestExists = false }
    let actualPut = List.map (createPutItem original.Path dest.Path) [
        createFile "/c/fruit/amazing/banana"
        createFile "/c/fruit/bad/grapefruit"
        createFile "/c/fruit/cherry"
        createFile "/c/fruit/dewberry"
    ]
    let putType = if wasCopy then Copy else Move
    let action = PutItems (putType, putItem, actualPut, false)
    let errorItem = actualPut.[1]
    fs.AddExnPath true ex errorItem.Dest
    let model =
        testModel
        |> withLocation "/d"
        |> pushUndo action
        |> withHistoryPaths (historyPaths {
            dest
            actualPut.[0].Dest.Parent, true
            actualPut.[0].Dest, false
            actualPut.[1].Dest, false
            actualPut.[2].Dest, false
        })

    let actual = seqResultWithCancelTokenCallback (fs.CancelAfterWriteCount 2) (Action.undo fs progress) model

    let expectedSuccessItems = [
        actualPut.[0]
        actualPut.[2]
    ]
    let expectedUndoAction = PutItems (putType, putItem, actualPut |> List.except expectedSuccessItems, true)
    let redoIntent = if wasCopy then putItem else putItem |> withDestExists
    let expectedRedoAction = PutItems (putType, redoIntent, expectedSuccessItems, true)
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
            Directory = expectedItems |> sortByPath
            Items = expectedItems
            Cursor = 0
            UndoStack = expectedUndoAction :: testModel.UndoStack
            RedoStack = expectedRedoAction :: testModel.RedoStack
        }.WithError (MainStatus.PutError (true, putType, [(errorItem |> PutItem.reverse).Item, ex], actualPut.Length))
        |> fun model ->
            if wasCopy
            then model // undo copy does not open a path, only refreshes if current path is destination
            else model.WithPushedLocation original.Path.Parent
        |> withHistoryPaths (historyPaths {
            dest
            if not wasCopy then
                actualPut.[0].Item
            actualPut.[1].Dest, false
            if not wasCopy then
                actualPut.[2].Item
        })
        |> withLocationOnHistory
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        drive 'c' [
            if wasCopy then
                itemTree
            else
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
    let putItem = { Item = src; Dest = dest; DestExists = false }
    let actualPut = [
        createFile "/c/folder/file" |> createPutItem src.Path dest
    ]
    let putType = if copy then Copy else Move
    let action = PutItems (putType, putItem, actualPut, false)
    let model = testModel |> withLocation "/d" |> pushRedo action

    let actual = seqResult (Action.redo fs progress) model

    let expectedActual = actualPut @ [
        createFile "/c/folder/new" |> createPutItem src.Path dest
    ]
    let expectedAction = PutItems (putType, putItem, expectedActual, false)
    let expectedItems = [
        createFolder "/d/dest/another"
        createFolder "/d/dest/folder"
    ]
    let expected =
        { model.WithPushedLocation dest.Parent with
            Directory = expectedItems |> sortByPath
            Items = expectedItems
            Cursor = 1
            UndoStack = expectedAction :: model.UndoStack
            RedoStack = model.RedoStack.Tail
            CancelToken = CancelToken()
        }.WithMessage (MainStatus.RedoAction (expectedAction, testModel.PathFormat, 1))
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

[<TestCaseSource("putTypeCases")>]
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
    let putItem = { Item = src; Dest = dest; DestExists = false }
    let actualPut =
        if putType = Shortcut then
            [putItem]
        else
            List.map (createPutItem src.Path dest) [
                createFile "/c/put/file"
                createFile "/c/put/other"
            ]
    let action = PutItems (putType, putItem, actualPut, false)
    let model = testModel |> pushRedo action
    let expectedFs = fs.Items

    let actual = seqResult (Action.redo fs progress) model

    let expectedError = MainStatus.CannotRedoPutToExisting (putType, src, dest.Format model.PathFormat)
    let expectedItems = [
        createFolder "/c/dest/other"
        if putType = Shortcut then
            createFile "/c/dest/put.lnk"
        else
            createFolder "/c/dest/put"
    ]
    let expected =
        { model.WithPushedLocation(dest.Parent) with
            Directory = expectedItems |> sortByPath
            Items = expectedItems
            Cursor = 1
            RedoStack = model.RedoStack.Tail
        }.WithError expectedError
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
        testModel.WithLocation dest.Path.Parent
        |> withReg (Some (src.Path, src.Type, Move))
        |> withHistoryPaths (historyPaths {
            "/c/folder/file"
            "/c/dest2/unrelated"
            "/c/folder/sub/"
            src
        })

    let actual = seqResult (Action.put fs progress enumerated) model

    let intent = { Item = src; Dest = dest.Path; DestExists = enumerated }
    let expectedPut =
        if enumerated then
            List.map (createPutItem src.Path dest.Path) [
                createFile "/c/folder/file"
                createFolder "/c/folder/sub"
            ]
        else
            [intent]
    let expectedAction = PutItems (Move, intent, expectedPut, false)
    let expectedItems = [dest]
    let expected =
        { testModel.WithLocation dest.Path.Parent with
            Directory = expectedItems |> sortByPath
            Items = expectedItems
            UndoStack = expectedAction :: testModel.UndoStack
            RedoStack = []
            CancelToken = CancelToken()
        }
        |> fun m ->
            if enumerated && deleteError
            then m.WithError (MainStatus.CouldNotDeleteMoveSource (src.Name, ex))
            else m.WithMessage (MainStatus.ActionComplete (expectedAction, testModel.PathFormat))
        |> withHistoryPaths (historyPaths {
            "/c/dest/folder/file"
            "/c/dest2/unrelated"
            "/c/dest/folder/sub/"
            intent.Dest, true
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
    let model = testModel |> withReg (Some (src.Path, src.Type, Move))
    let expectedFs = fs.Items

    let actual = seqResult (Action.put fs progress false) model

    let expected = model.WithError MainStatus.CannotMoveToSameFolder
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
    let putItem = { Item = original; Dest = moved.Path; DestExists = false }
    let action = PutItems (Move, putItem, [putItem], false)
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
            Directory = expectedItems |> sortByPath
            Items = expectedItems
            Cursor = 3
            UndoStack = model.UndoStack.Tail
            RedoStack = action :: model.RedoStack
            CancelToken = CancelToken()
        }.WithMessage (MainStatus.UndoAction (action, model.PathFormat, 1))
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
let ``Undo move of enumerated folder deletes original dest folder when empty afterward`` destFolderEmpty =
    let fs = FakeFileSystem [
        drive 'c' [
            folder "dest" [
                folder "moved" [
                    file "file"
                    if not destFolderEmpty then
                        file "new"
                    file "other"
                ]
            ]
        ]
        drive 'd' [
            folder "another" []
        ]
    ]
    let moved = fs.Item "/c/dest/moved"
    let original = createFolder "/d/moved"
    let putItem = { Item = original; Dest = moved.Path; DestExists = false }
    let actualMoved = List.map (createPutItem original.Path moved.Path) [
        createFile "/d/moved/file"
        createFile "/d/moved/other"
    ]
    let action = PutItems (Move, putItem, actualMoved, false)
    let model =
        testModel
        |> pushUndo action
        |> withHistoryPaths (historyPaths {
            moved
            actualMoved.[0].Dest, false
            actualMoved.[1].Item
            if not destFolderEmpty then
                moved.Path.Join "new", false
        })

    let actual = seqResult (Action.undo fs progress) model

    let expectedItems = [
        createFolder "/d/another"
        createFolder "/d/moved"
    ]
    let expected =
        { model.WithPushedLocation original.Path.Parent with
            Directory = expectedItems |> sortByPath
            Items = expectedItems
            Cursor = 1
            UndoStack = model.UndoStack.Tail
            RedoStack = action :: model.RedoStack
            CancelToken = CancelToken()
        }.WithMessage (MainStatus.UndoAction (action, testModel.PathFormat, 1))
        |> withHistoryPaths (historyPaths {
            original
            actualMoved.[0].Item
            actualMoved.[1].Item
            if not destFolderEmpty then
                original.Path.Join "new", false
        })
        |> withLocationOnHistory
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        drive 'c' [
            folder "dest" [
                if not destFolderEmpty then
                    folder "moved" [
                        file "new"
                    ]
            ]
        ]
        drive 'd' [
            folder "another" []
            folder "moved" [
                file "file"
                file "other"
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
    let moved = fs.Item "/c/dest/moved"
    let original = createFolder "/c/moved"
    let putItem = { Item = original; Dest = moved.Path; DestExists = true }
    let createPutItem = createPutItem original.Path moved.Path
    let actualMoved = [
        createFolder "/c/moved/folder" |> createPutItem |> withDestExists
        createFile "/c/moved/file" |> createPutItem |> withDestExists
        createFile "/c/moved/other" |> createPutItem
    ]
    let action = PutItems (Move, putItem, actualMoved, false)
    let model =
        testModel
        |> pushUndo action
        |> withHistoryPaths (historyPaths {
            moved
            actualMoved.[0].Dest, false
            actualMoved.[1].Dest, false
            actualMoved.[2].Dest, false
        })

    let actual = seqResult (Action.undo fs progress) model

    let expectedItems = [
        createFolder "/c/dest"
        createFolder "/c/moved"
    ]
    let expected =
        { model with
            Directory = expectedItems |> sortByPath
            Items = expectedItems
            Cursor = 1
            UndoStack = model.UndoStack.Tail
            RedoStack = action :: testModel.RedoStack
            CancelToken = CancelToken()
        }.WithMessage (MainStatus.UndoAction (action, testModel.PathFormat, 1))
        |> withHistoryPaths (historyPaths {
            moved
            actualMoved.[0].Dest, false
            actualMoved.[1].Dest, false
            actualMoved.[2].Item
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
        folder "dest" [
            folder "moved" [
                file "file"
                file "other"
            ]
        ]
        if errorCausedByExisting then
            folder "moved" [
                file "file"
            ]
    ]
    let errorItem = createFile "/c/dest/moved/file"
    if not errorCausedByExisting then
        fs.AddExnPath false ex errorItem.Path
    let destPath = createPath "/c/dest/moved"
    let original = createFolder "/c/moved"
    let putItem = { Item = original; Dest = destPath; DestExists = destExisted }
    let actualMoved = List.map (createPutItem original.Path destPath) [
        createFile "/c/moved/file"
        createFile "/c/moved/other"
    ]
    let action = PutItems (Move, putItem, actualMoved, false)
    let model =
        testModel
        |> pushUndo action
        |> withHistoryPaths (historyPaths {
            errorItem
            "/c/dest2/unrelated"
            destPath, true
            actualMoved.[1].Dest, false
        })

    let actual = seqResult (Action.undo fs progress) model

    let expectedItems = [
        createFolder "/c/dest"
        original
    ]
    let expectedExn = if errorCausedByExisting then UndoMoveBlockedByExistingItemException() :> exn else ex
    let expectedError = MainStatus.PutError (true, Move, [(errorItem, expectedExn)], 2)
    // intent DestExists should be set to true so that redo would merge, otherwise it would always return CannotRedoPutToExisting
    let expectedAction = PutItems (Move, putItem |> withDestExists, actualMoved |> List.skip 1, false)
    let expected =
        { model with
            Directory = expectedItems |> sortByPath
            Items = expectedItems
            Cursor = 1
            UndoStack = model.UndoStack.Tail
            RedoStack = expectedAction :: testModel.RedoStack
            CancelToken = CancelToken()
        }.WithError expectedError
        |> withHistoryPaths (historyPaths {
            model.Location, true
            yield! model.History.Paths |> List.take 3
            actualMoved.[1].Item
        })
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        folder "dest" [
            folder "moved" [
                file "file"
            ]
        ]
        folder "moved" [
            if errorCausedByExisting then
                file "file"
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
    let putItem = { Item = original; Dest = moved.Path; DestExists = false }
    let action = PutItems (Move, putItem, [putItem], false)
    let model = testModel |> withLocation "/c/other" |> pushUndo action
    let expectedFs = fs.Items

    let actual = seqResult (Action.undo fs progress) model

    let expectedError = MainStatus.PutError (true, Move, [(moved, UndoMoveBlockedByExistingItemException() :> exn)], 1)
    let expected = model.WithError expectedError |> popUndo |> withNewCancelToken
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
    let putItem = { Item = original; Dest = moved.Path; DestExists = false }
    let action = PutItems (Move, putItem, [putItem], false)
    let model = testModel |> withLocation "/c/other" |> pushUndo action
    let expectedFs = fs.Items

    let actual = seqResult (Action.undo fs progress) model

    let expectedError = MainStatus.PutError (true, Move, [(moved, ex)], 1)
    let expected = model.WithError expectedError |> popUndo |> withNewCancelToken
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
    let putItem = { Item = src; Dest = destPath; DestExists = true }
    let actualMoved = List.map (createPutItem src.Path destPath) [
        createFile "/c/moved/file" |> size 2L
        createFile "/c/moved/other"
    ]
    let action = PutItems (Move, putItem, actualMoved, false)
    let model = testModel |> pushRedo action
    let expectedFile = fs.Item "/c/moved/file"

    let actual = seqResult (Action.redo fs progress) model

    let expectedItems = [
        createFolder "/c/dest/another"
        createFolder "/c/dest/moved"
    ]
    let expectedActual = [
        // even though this file was not originally an overwrite, a redo should overwrite since we're redoing the intent
        // to merge the folder
        expectedFile |> createPutItem src.Path destPath |> withDestExists
        actualMoved.[1]
    ]
    let expectedAction = PutItems (Move, putItem, expectedActual, false)
    let expected =
        { model.WithPushedLocation(destPath.Parent) with
            Directory = expectedItems |> sortByPath
            Items = expectedItems
            Cursor = 1
            UndoStack = expectedAction :: model.UndoStack
            RedoStack = model.RedoStack.Tail
            CancelToken = CancelToken()
        }.WithMessage (MainStatus.RedoAction (expectedAction, model.PathFormat, 1))
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
let ``Put item to copy in same folder calls file sys copy with new name`` existingCopies =
    let fs = FakeFileSystem [
        file "file"
        yield! copyNames "file" existingCopies |> List.map file
    ]
    let src = fs.Item "/c/file"
    let model = testModel |> withReg (Some (src.Path, src.Type, Copy))

    let actual = seqResult (Action.put fs progress false) model

    let expectedItems =
        [
            createFile "/c/file"
            yield! copyNames "file" (existingCopies+1) |> List.map (fun name -> createFile ("/c/" + name))
        ] |> sortByPath
    let expectedPath = createPath ("/c/" + copyName "file" existingCopies)
    let putItem = { Item = createFile "/c/file"; Dest = expectedPath; DestExists = false }
    let expectedAction = PutItems (Copy, putItem, [putItem], false)
    let expected =
        { testModel with
            Directory = expectedItems
            Items = expectedItems
            Cursor = expectedItems |> List.findIndex (fun i -> i.Path = expectedPath)
            UndoStack = expectedAction :: testModel.UndoStack
            RedoStack = []
            CancelToken = CancelToken()
        }.WithMessage (MainStatus.ActionComplete (expectedAction, model.PathFormat))
        |> withLocationOnHistory
    assertAreEqual expected actual

// redo copy tests

[<Test>]
let ``Redo copy item to same parent returns error`` () =
    let copyName = Action.getCopyName "file" 0
    let fs = FakeFileSystem [
        folder "folder" [
            file "another"
            file "file"
            file copyName
        ]
    ]
    let src = fs.Item "/c/folder/file"
    let dest = createPath ("/c/folder/" + copyName)
    let putItem = { Item = src; Dest = dest; DestExists = false }
    let action = PutItems (Copy, putItem, [putItem], false)
    let model = testModel |> pushRedo action
    let expectedFs = fs.Items
    let expectedItems = fs.ItemsIn "/c/folder"

    let actual = seqResult (Action.redo fs progress) model

    let expectedError = MainStatus.CannotRedoPutToExisting (Copy, src, dest.Format model.PathFormat)
    let expected =
        { model.WithPushedLocation(dest.Parent) with
            Directory = expectedItems |> sortByPath
            Items = expectedItems
            Cursor = 2
            RedoStack = model.RedoStack.Tail
        }.WithError expectedError
        |> withLocationOnHistory
    assertAreEqual expected actual
    fs.Items |> shouldEqual expectedFs

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
        folder otherCopyName [] // to make sure redo doesn't try to re-determine copy name on its own
        folder copyName [
            folder "amazing" [
                fileWith (size 7L) "banana" // simulate copied file changing, which should not affect resuming
            ]
            file "apple"
        ]
    ]
    let src = fs.Item "/c/fruit"
    let dest = createFolder ("/c/" + copyName)
    let putItem = { Item = src; Dest = dest.Path; DestExists = false }
    let createPutItem = createPutItem src.Path dest.Path
    let undoActualPut = List.map createPutItem [
        createFile "/c/fruit/amazing/banana" |> size 2L
        createFile "/c/fruit/apple"
    ]
    let redoActualPut = List.map createPutItem [
        createFile "/c/fruit/cherry"
        createFile "/c/fruit/dewberry"
    ]
    let model =
        testModel
        |> pushUndo (PutItems (Copy, putItem, undoActualPut, true))
        |> pushRedo (PutItems (Copy, putItem, redoActualPut, true))

    let actual = seqResult (Action.redo fs progress) model

    let expectedStatusAction = PutItems (Copy, putItem |> withDestExists, redoActualPut, false)
    let expectedMergedAction = PutItems (Copy, putItem, undoActualPut @ redoActualPut, false)
    let expectedItems = [
        src
        createFolder ("/c/" + otherCopyName)
        dest
    ]
    let expected =
        { model with
            Directory = expectedItems |> sortByPath
            Items = expectedItems
            Cursor = 2
            UndoStack = expectedMergedAction :: testModel.UndoStack
            RedoStack = testModel.RedoStack
            CancelToken = CancelToken()
        }
        |> withReg None
        |> fun model -> model.WithMessage (MainStatus.RedoAction (expectedStatusAction, model.PathFormat, 1))
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

[<TestCase(false, true)>]
[<TestCase(false, false)>]
[<TestCase(false, null)>]
[<TestCase(true, true)>]
[<TestCase(true, false)>]
[<TestCase(true, null)>]
let ``Undo copy file deletes when it has the same timestamp or recycles otherwise`` curPathDifferent (sameTimestamp: bool Nullable) =
    let time = DateTime(2000, 1, 1)
    let sameTimestamp = sameTimestamp |> Option.ofNullable
    let copyTime = sameTimestamp |> Option.map (fun same -> if same then time else time.AddDays(1.0))
    let fs = FakeFileSystem [
        driveWithSize 'c' 100L [
            folder "other" []
            folder "src" [
                fileWith (modified time) "file"
            ]
            fileWith (modifiedOpt copyTime >> size 1L) "file"
        ]
    ]
    let original = fs.Item "/c/src/file"
    let copied = fs.Item "/c/file"
    let putItem = { Item = original; Dest = copied.Path; DestExists = false }
    let action = PutItems (Copy, putItem, [putItem], false)
    let location = if curPathDifferent then "/c/other" else "/c"
    let model = testModel |> withLocation location |> pushUndo action

    let actual = seqResult (Action.undo fs progress) model

    let expectedItems = [
        createFolder "/c/other"
        createFolder "/c/src"
    ]
    let expected =
        { model with
            Directory = if curPathDifferent then model.Directory else expectedItems |> sortByPath
            Items = if curPathDifferent then model.Items else expectedItems
            UndoStack = model.UndoStack.Tail
            RedoStack = action :: model.RedoStack
            CancelToken = CancelToken()
        }.WithMessage (MainStatus.UndoAction (action, model.PathFormat, 1))
        |> if not curPathDifferent then withLocationOnHistory else id
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        driveWithSize 'c' 100L [
            folder "other" []
            folder "src" [
                fileWith (modified time) "file"
            ]
        ]
    ]
    fs.RecycleBin |> shouldEqual [
        if sameTimestamp <> Some true then
            copied
    ]

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
    let putItem = { Item = original; Dest = copied.Path; DestExists = false }
    let action = PutItems (Copy, putItem, [putItem], false)
    let model = testModel |> pushUndo action

    let actual = seqResult (Action.undo fs progress) model

    let expected =
        { model with
            UndoStack = model.UndoStack.Tail
            RedoStack = action :: model.RedoStack
            CancelToken = CancelToken()
        }.WithMessage (MainStatus.UndoAction (action, model.PathFormat, 1))
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
    let putItem = { Item = original; Dest = copied.Path; DestExists = false }
    let action = PutItems (Copy, putItem, [putItem], false)
    let model = testModel |> pushUndo action

    let actual = seqResult (Action.undo fs progress) model

    let expectedExn = FakeFileSystemErrors.cannotDeleteNonEmptyFolder
    let expected =
        { model with
            UndoStack = model.UndoStack.Tail
            CancelToken = CancelToken()
        }.WithError (MainStatus.PutError (true, Copy, [copied, expectedExn], 1))
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

[<TestCase(false, false)>]
[<TestCase(false, true)>]
[<TestCase(true, false)>]
[<TestCase(true, true)>]
let ``Undo copy folder deletes items that were copied and removes dest folders if empty`` hasNewItem isLocationDest =
    let fs = FakeFileSystem [
        driveWithSize 'c' 100L [
            folder "copied" [
                folder "folder" [
                    file "sub"
                ]
                file "file"
                file "other"
            ]
            folder "dest" [
                folder "another" []
                folder "copied" [
                    folder "folder" [
                        file "sub"
                    ]
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
    let putItem = { Item = original; Dest = copied.Path; DestExists = false }
    let actualCopied = List.map (createPutItem original.Path copied.Path) [
        createFile "/c/copied/folder/sub"
        createFile "/c/copied/file"
        createFile "/c/copied/other"
    ]
    let action = PutItems (Copy, putItem, actualCopied, false)
    let model =
        testModel
        |> pushUndo action
        |> (if isLocationDest then withLocation "/c/dest" else id)
        |> withHistoryPaths (historyPaths {
            copied
            actualCopied.[0].Dest.Parent, false
            actualCopied.[0].Dest, false
            actualCopied.[1].Dest, false
            if hasNewItem then
                copied.Path.Join "new", false
        })

    let actual = seqResult (Action.undo fs progress) model

    let expectedItems =
        if not isLocationDest then
            testModel.Items
        else
            [
                createFolder "/c/dest/another"
                if hasNewItem then
                    copied
            ]
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            CancelToken = CancelToken()
        }.WithMessage (MainStatus.UndoAction (action, model.PathFormat, 1))
        |> popUndo
        |> pushRedo action
        |> withHistoryPaths (historyPaths {
            if isLocationDest then
                model.Location, true
            if hasNewItem then
                copied
                copied.Path.Join "new", false
        })
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        driveWithSize 'c' 100L [
            folder "copied" [
                folder "folder" [
                    file "sub"
                ]
                file "file"
                file "other"
            ]
            folder "dest" [
                folder "another" []
                if hasNewItem then
                    folder "copied" [
                        file "new"
                    ]
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
    let putItem = { Item = original; Dest = copied.Path; DestExists = true }
    let createPutItem = createPutItem original.Path copied.Path
    let actualCopied = [
        createFile "/c/copied/file" |> createPutItem |> withDestExists
        createFile "/c/copied/other" |> createPutItem
    ]
    let action = PutItems (Copy, putItem, actualCopied, false)
    let model =
        testModel
        |> pushUndo action
        |> withHistoryPaths (historyPaths {
            copied
            actualCopied.[0].Dest, false
            actualCopied.[1].Dest, false
        })

    let actual = seqResult (Action.undo fs progress) model

    let expectedAction = PutItems (Copy, putItem, actualCopied |> List.filter (fun pi -> not pi.DestExists), false)
    let expected =
        { model with
            UndoStack = model.UndoStack.Tail
            RedoStack = expectedAction :: testModel.RedoStack
            CancelToken = CancelToken()
        }.WithMessage (MainStatus.UndoAction (expectedAction, testModel.PathFormat, 1))
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

[<TestCase(false, false)>] // error is during deleting an item
[<TestCase(false, true)>] // error is during deleting an item and we have same copy operation on redo stack
[<TestCase(true, false)>] // error is during deleting empty original destination folder
let ``Undo copy handles partial success by updating redo and setting error message`` errorIsDeleteDest hasCancelledRedoItem =
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
    let errorItem =
        if errorIsDeleteDest
        then copied
        else fs.Item "/c/dest/copied/file"
    fs.AddExnPath true ex errorItem.Path
    let putItem = { Item = original; Dest = copied.Path; DestExists = false }
    let actualCopied = List.map (createPutItem original.Path copied.Path) [
        createFile "/c/copied/file"
        createFile "/c/copied/other"
    ]
    let action = PutItems (Copy, putItem, actualCopied, false)
    let prevCopyItems = [createFile "/c/copied/prev" |> createPutItem original.Path copied.Path]
    let model =
        testModel
        |> pushUndo action
        |> if hasCancelledRedoItem
            then pushRedo (PutItems (Copy, putItem, prevCopyItems, true))
            else id
        |> withHistoryPaths (historyPaths {
            copied
            actualCopied.[0].Dest, false
            actualCopied.[1].Dest, false
        })

    let actual = seqResult (Action.undo fs progress) model

    let expectedError =
        if errorIsDeleteDest
        then MainStatus.CouldNotDeleteCopyDest (copied.Name, ex)
        else MainStatus.PutError (true, Copy, [errorItem, ex], 2)
    let expectedActual =
        if hasCancelledRedoItem then prevCopyItems else []
        @ actualCopied |> List.filter (fun pi -> pi.Dest <> errorItem.Path)
    let expectedAction = PutItems (Copy, putItem, expectedActual, false)
    let expected =
        { model with
            UndoStack = model.UndoStack.Tail
            RedoStack = expectedAction :: testModel.RedoStack
            CancelToken = CancelToken()
        }.WithError expectedError
        |> withHistoryPaths [
            model.History.Paths.[0]
            if not errorIsDeleteDest then
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
                    if not errorIsDeleteDest then
                        file "file"
                ]
            ]
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
    let putItem = { Item = original; Dest = copied.Path; DestExists = false }
    fs.AddExnPath false (exn "GetItem error") copied.Path
    fs.AddExnPath false ex copied.Path
    let action = PutItems (Copy, putItem, [putItem], false)
    let model = testModel |> pushUndo action
    let expectedFs = fs.Items

    let actual = seqResult (Action.undo fs progress) model

    let expectedError = MainStatus.PutError (true, Copy, [copied, ex], 1)
    let expected = model.WithError expectedError |> popUndo |> withNewCancelToken
    assertAreEqual expected actual
    fs.Items |> shouldEqual expectedFs
    fs.RecycleBin |> shouldEqual []

// shortcut tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Put shortcut calls file sys create shortcut`` overwrite =
    let fs = FakeFileSystem [
        folder "src" [
            file "file"
        ]
        if overwrite then
            file "file.lnk"
    ]
    let target = fs.Item "/c/src/file"
    let shortcut = createFile "/c/file.lnk"
    let model = testModel |> withReg (Some (target.Path, target.Type, Shortcut))

    let actual = seqResult (Action.put fs progress overwrite) model

    let expectedPutItem = { Item = target; Dest = shortcut.Path; DestExists = overwrite }
    let expectedAction = PutItems (Shortcut, expectedPutItem, [expectedPutItem], false)
    let expectedItems = [
        createFolder "/c/src"
        shortcut
    ]
    let expected =
        { testModel with
            Directory = expectedItems |> sortByPath
            Items = expectedItems
            Cursor = 1
            UndoStack = expectedAction :: testModel.UndoStack
            RedoStack = []
            CancelToken = CancelToken()
        }.WithMessage (MainStatus.ActionComplete (expectedAction, model.PathFormat))
        |> withLocationOnHistory
    assertAreEqual expected actual
    fs.GetShortcutTarget shortcut.Path |> shouldEqual (Ok (string target.Path))

// undo shortcut tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo create shortcut deletes shortcut`` curPathDifferent =
    let fs = FakeFileSystem [
        folder "other" []
        file "file.lnk"
    ]
    let shortcut = fs.Item "/c/file.lnk"
    let location = if curPathDifferent then "/c/other" else "/c"
    let model = testModel |> withLocation location |> withHistoryPaths [itemHistoryPath shortcut]

    let actual = Action.undoShortcut fs shortcut.Path model
                 |> assertOk

    let expectedItems = [
        createFolder "/c/other"
    ]
    let expected =
        if curPathDifferent then
            model |> withHistoryPaths []
        else
            { model with
                Directory = expectedItems |> sortByPath
                Items = expectedItems
                Cursor = 0
            }
            |> withHistoryPaths []
            |> withLocationOnHistory
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        folder "other" []
    ]
    fs.RecycleBin |> shouldEqual []

[<Test>]
let ``Undo create shortcut handles errors by returning error and consuming action`` () =
    let fs = FakeFileSystem [
        file "file.lnk"
    ]
    let shortcut = fs.Item "/c/file.lnk"
    fs.AddExnPath false ex shortcut.Path
    let model = testModel

    let actual = Action.undoShortcut fs shortcut.Path model

    let action = DeletedItem (shortcut, true)
    let expected = Error (MainStatus.ItemActionError (action, model.PathFormat, ex))
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        file "file.lnk"
    ]
