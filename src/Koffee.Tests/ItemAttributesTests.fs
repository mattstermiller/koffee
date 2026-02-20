module Koffee.ItemAttributesTests

open NUnit.Framework
open FsUnitTyped
open Acadian.FSharp
open Koffee.ItemActionCommands

[<TestCase(false, false)>]
[<TestCase(false, true)>]
[<TestCase(true, false)>]
[<TestCase(true, true)>]
let ``toggleHidden and redo on non-hidden items sets them to hidden`` isRedo showHidden =
    let fs = FakeFileSystem [
        file "file1"
        file "file2"
        file "file3"
        fileWith (hide true) "hidden"
        file "other1"
        file "other2"
        file "other3"
    ]
    let items = fs.ItemsIn "/c"
    let actionItems = items |> List.take 3
    let action = ToggleHidden (true, actionItems, false)
    let shownItems = items |> applyIf (not showHidden) (List.filter (not << _.IsHidden))
    let model =
        { testModel with
            Directory = items
            Items = shownItems
            SelectedItems = actionItems
            Cursor = shownItems |> List.findIndex (fun i -> i.Name = "other2")
            MainModel.Config.ShowHidden = showHidden
        }
        |> applyIf isRedo (pushRedo action)
    let testFunc =
        if isRedo
        then Undo.redo fs progress
        else Attributes.toggleHidden fs progress

    let actual = seqResult testFunc model

    let expectedItems = [
        createFile "/c/file1" |> hide true
        createFile "/c/file2" |> hide true
        createFile "/c/file3" |> hide true
        createFile "/c/hidden" |> hide true
        createFile "/c/other1"
        createFile "/c/other2"
        createFile "/c/other3"
    ]
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems |> applyIf (not showHidden) (List.filter (not << _.IsHidden))
            SelectedItems = if showHidden then expectedItems |> List.take 3 else []
            Cursor = if showHidden then 0 else 1
            UndoStack = action :: model.UndoStack
            RedoStack = if isRedo then model.RedoStack.Tail else []
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (
            if isRedo
            then MainStatus.RedoAction (action, 1, 1)
            else MainStatus.ActionComplete action
        )
    actual |> assertAreEqual expected
    fs.ItemsIn "/c" |> assertAreEqual expectedItems

[<Test>]
let ``toggleHidden on mixed hidden items sets all to hidden`` () =
    let fs = FakeFileSystem [
        fileWith (hide true) "file1"
        file "file2"
        fileWith (hide true) "file3"
        file "other"
    ]
    let items = fs.ItemsIn "/c"
    let actionItems = items |> List.take 3
    let model =
        { testModel with
            Directory = items
            Items = items
            SelectedItems = actionItems
            MainModel.Config.ShowHidden = true
        }

    let actual = seqResult (Attributes.toggleHidden fs progress) model

    let expectedItems = [
        createFile "/c/file1" |> hide true
        createFile "/c/file2" |> hide true
        createFile "/c/file3" |> hide true
        createFile "/c/other"
    ]
    let expectedAction = ToggleHidden (true, actionItems, false)
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            SelectedItems = actionItems |> List.map (hide true)
            UndoStack = expectedAction :: model.UndoStack
            RedoStack = []
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (MainStatus.ActionComplete expectedAction)
    actual |> assertAreEqual expected
    fs.ItemsIn "/c" |> assertAreEqual expectedItems

[<TestCase(false)>]
[<TestCase(true)>]
let ``toggleHidden and redo on hidden items sets them to non-hidden`` isRedo =
    let fs = FakeFileSystem [
        fileWith (hide true) "file1"
        fileWith (hide true) "file2"
        fileWith (hide true) "file3"
        file "other"
    ]
    let items = fs.ItemsIn "/c"
    let actionItems = items |> List.take 3
    let action = ToggleHidden (false, actionItems, false)
    let model =
        { testModel with
            Directory = items
            Items = items
            SelectedItems = actionItems
            MainModel.Config.ShowHidden = true
        }
        |> applyIf isRedo (pushRedo action)
    let testFunc =
        if isRedo
        then Undo.redo fs progress
        else Attributes.toggleHidden fs progress

    let actual = seqResult testFunc model

    let expectedItems = [
        createFile "/c/file1"
        createFile "/c/file2"
        createFile "/c/file3"
        createFile "/c/other"
    ]
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            SelectedItems = expectedItems |> List.take 3
            UndoStack = action :: model.UndoStack
            RedoStack = if isRedo then model.RedoStack.Tail else []
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (
            if isRedo
            then MainStatus.RedoAction (action, 1, 1)
            else MainStatus.ActionComplete action
        )
    actual |> assertAreEqual expected
    fs.ItemsIn "/c" |> assertAreEqual expectedItems

[<TestCase(false)>]
[<TestCase(true)>]
let ``toggleHidden and redo handles error by setting error status and redo`` isRedo =
    let fs = FakeFileSystem [
        file "file"
        file "other"
    ]
    let item = fs.Item "/c/file"
    fs.AddExnPath true ex item.Path
    let items = fs.ItemsIn "/c"
    let action = ToggleHidden (true, [item], false)
    let model =
        { testModel with
            Directory = items
            Items = items
        }
        |> applyIf isRedo (pushRedo action)
    let testFunc =
        if isRedo
        then Undo.redo
        else Attributes.toggleHidden

    let actual = seqResult (testFunc fs progress) model

    let expectedError = MainStatus.ToggleHiddenError (true, [createPath "/c/file", ex], 1)
    let expectedRedo = ToggleHidden (true, [item], true)
    let expected =
        model
        |> MainModel.withRedoStack (expectedRedo :: if isRedo then model.RedoStack.Tail else [])
        |> withNewCancelToken
        |> MainModel.withError expectedError
    actual |> assertAreEqual expected

[<TestCase(false)>]
[<TestCase(true)>]
let ``toggleHidden and redo with partial success sets error message, then redo retries and merges undo item`` isRedo =
    let fs = FakeFileSystem [
        file "file1"
        file "file2"
        file "file3"
        fileWith (hide true) "hidden"
        file "other"
    ]
    let errorItem1 = fs.Item (createPath "/c/file1")
    let errorItem2 = fs.Item (createPath "/c/file2")
    let successful = fs.Item (createPath "/c/file3")
    let items = fs.ItemsIn "/c"
    let actionItems = items |> List.take 3
    let action = ToggleHidden (true, actionItems, false)
    let model =
        { testModel with
            Directory = items
            Items = items
            SelectedItems = actionItems
        }
        |> applyIf isRedo (pushRedo action)
    let testFunc =
        if isRedo
        then Undo.redo
        else Attributes.toggleHidden

    let assertErrorModel singleError actual =
        let errorItems = [
            if not singleError then
                errorItem1
            errorItem2
        ]
        let expectedSuccessful = [
            successful
            if singleError then
                errorItem1
        ]
        let expectedItems = [
            createFile "/c/file1" |> hide singleError
            createFile "/c/file2"
            createFile "/c/file3" |> hide true
            createFile "/c/hidden" |> hide true
            createFile "/c/other"
        ]
        let expectedErrorPaths = errorItems |> List.map (fun i -> i.Path, ex)
        let expectedError = MainStatus.ToggleHiddenError (true, expectedErrorPaths, errorItems.Length + 1)
        let expectedUndo = ToggleHidden (true, expectedSuccessful, true)
        let expectedRedo = ToggleHidden (true, errorItems, true)
        let expected =
            { model with
                Directory = expectedItems
                Items = expectedItems |> List.filter (not << _.IsHidden)
                SelectedItems =
                    if singleError
                    then [] // selected list isn't set for only one item since cursor is sufficient
                    else errorItems
                UndoStack = expectedUndo :: model.UndoStack
                RedoStack = expectedRedo :: (if isRedo then model.RedoStack.Tail else [])
                CancelToken = CancelToken()
            }
            |> MainModel.withError expectedError
        actual |> assertAreEqual expected
        fs.ItemsIn "/c" |> assertAreEqual expectedItems

    // part one: handles errors
    fs.AddExnPath true ex errorItem1.Path
    fs.AddExnPath true ex errorItem2.Path
    let modelAfterError =
        model
        |> seqResult (testFunc fs progress)
        |>! assertErrorModel false

    // part two: redo should retry and handle error
    fs.AddExnPath true ex errorItem2.Path
    let modelAfterRetry =
        modelAfterError
        |> seqResult (Undo.redo fs progress)
        |>! assertErrorModel true

    // part three: redo with no error should complete the operation and merge undo item
    let actual = modelAfterRetry |> seqResult (Undo.redo fs progress)

    let expectedItems = [
        createFile "/c/file1" |> hide true
        createFile "/c/file2" |> hide true
        createFile "/c/file3" |> hide true
        createFile "/c/hidden" |> hide true
        createFile "/c/other"
    ]
    let expectedUndo = ToggleHidden (true, [successful; errorItem1; errorItem2], false)
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems |> List.filter (not << _.IsHidden)
            SelectedItems = []
            UndoStack = expectedUndo :: model.UndoStack
            RedoStack = if isRedo then model.RedoStack.Tail else []
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (MainStatus.RedoAction (ToggleHidden (true, [errorItem2], false), 1, 1))
    actual |> assertAreEqual expected
    fs.ItemsIn "/c" |> assertAreEqual expectedItems

[<TestCase(false)>]
[<TestCase(true)>]
let ``toggleHidden and redo sets hidden until canceled, then redo resumes and merges undo item`` isRedo =
    let fs = FakeFileSystem [
        file "file1"
        file "file2"
        file "file3"
        file "file4"
        fileWith (hide true) "hidden"
        file "other"
    ]
    let items = fs.ItemsIn "/c"
    let actionItems = items |> List.take 4
    let action = ToggleHidden (true, actionItems, false)
    let model =
        { testModel with
            Directory = items
            Items = items
            SelectedItems = actionItems
        }
        |> applyIf isRedo (pushRedo action)
    let writesBeforeCancel = 2
    let testFunc =
        if isRedo
        then Undo.redo fs progress
        else Attributes.toggleHidden fs progress

    // part one: cancels correctly
    let modelAfterCancel = seqResultWithCancelTokenCallback (fs.CancelAfterWriteCount writesBeforeCancel) testFunc model

    let resumeItems = actionItems |> List.skip writesBeforeCancel
    (
        let expectedItems = [
            createFile "/c/file1" |> hide true
            createFile "/c/file2" |> hide true
            createFile "/c/file3"
            createFile "/c/file4"
            createFile "/c/hidden" |> hide true
            createFile "/c/other"
        ]
        let expectedAction = ToggleHidden (true, actionItems |> List.take writesBeforeCancel, true)
        let expectedRedo = ToggleHidden (true, resumeItems, true)
        let expected =
            { model with
                Directory = expectedItems
                Items = expectedItems |> List.filter (not << _.IsHidden)
                SelectedItems = resumeItems
                UndoStack = expectedAction :: model.UndoStack
                RedoStack = expectedRedo :: (if isRedo then model.RedoStack.Tail else [])
            }
            |> MainModel.withMessage (MainStatus.CancelledToggleHidden (true, false, writesBeforeCancel, actionItems.Length))
        modelAfterCancel |> assertAreEqual expected
        fs.ItemsIn "/c" |> assertAreEqual expectedItems
    )

    // part two: redo should complete the operation and merge undo item
    let actual = modelAfterCancel |> seqResult (Undo.redo fs progress)

    let expectedItems = [
        createFile "/c/file1" |> hide true
        createFile "/c/file2" |> hide true
        createFile "/c/file3" |> hide true
        createFile "/c/file4" |> hide true
        createFile "/c/hidden" |> hide true
        createFile "/c/other"
    ]
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems |> List.filter (not << _.IsHidden)
            SelectedItems = []
            UndoStack = action :: model.UndoStack
            RedoStack = if isRedo then model.RedoStack.Tail else []
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (MainStatus.RedoAction (ToggleHidden (true, resumeItems, false), 1, 1))
    actual |> assertAreEqual expected
    fs.ItemsIn "/c" |> assertAreEqual expectedItems

[<TestCase(false)>]
[<TestCase(true)>]
let ``undo toggleHidden on non-hidden items sets them back to non-hidden`` showHidden =
    let fs = FakeFileSystem [
        fileWith (hide true) "file1"
        fileWith (hide true) "file2"
        fileWith (hide true) "file3"
        fileWith (hide true) "hidden"
        file "other"
    ]
    let items = fs.ItemsIn "/c"
    let actionItems = items |> List.take 3 |> List.map (hide false)
    let action = ToggleHidden (true, actionItems, false)
    let model =
        { testModel with
            Directory = items
            Items = items |> applyIf (not showHidden) (List.filter (not << _.IsHidden))
            SelectedItems = actionItems
            MainModel.Config.ShowHidden = showHidden
        }
        |> pushUndo action

    let actual = seqResult (Undo.undo fs progress) model

    let expectedItems = [
        yield! actionItems
        createFile "/c/hidden" |> hide true
        createFile "/c/other"
    ]
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems |> applyIf (not showHidden) (List.filter (not << _.IsHidden))
            SelectedItems = actionItems
            CancelToken = CancelToken()
        }
        |> popUndo
        |> pushRedo action
        |> MainModel.withMessage (MainStatus.UndoAction (action, 1, 1))
    actual |> assertAreEqual expected
    fs.ItemsIn "/c" |> assertAreEqual expectedItems

[<TestCase(false)>]
[<TestCase(true)>]
let ``undo toggleHidden on hidden items sets them back to hidden`` showHidden =
    let fs = FakeFileSystem [
        file "file1"
        file "file2"
        file "file3"
        fileWith (hide true) "hidden"
        file "other"
    ]
    let items = fs.ItemsIn "/c"
    let actionItems = items |> List.take 3 |> List.map (hide true)
    let action = ToggleHidden (false, actionItems, false)
    let model =
        { testModel with
            Directory = items
            Items = items |> List.filter (not << _.IsHidden)
            MainModel.Config.ShowHidden = showHidden
        }
        |> pushUndo action

    let actual = seqResult (Undo.undo fs progress) model

    let expectedItems = [
        yield! actionItems
        createFile "/c/hidden" |> hide true
        createFile "/c/other"
    ]
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems |> applyIf (not showHidden) (List.filter (not << _.IsHidden))
            SelectedItems = if showHidden then actionItems else []
            CancelToken = CancelToken()
        }
        |> popUndo
        |> pushRedo action
        |> MainModel.withMessage (MainStatus.UndoAction (action, 1, 1))
    actual |> assertAreEqual expected
    fs.ItemsIn "/c" |> assertAreEqual expectedItems

[<Test>]
let ``undo toggleHidden handles error by setting error status and undo`` () =
    let fs = FakeFileSystem [
        file "file"
        file "other"
    ]
    let item = fs.Item "/c/file"
    fs.AddExnPath true ex item.Path
    let items = fs.ItemsIn "/c"
    let action = ToggleHidden (true, [item], false)
    let model =
        { testModel with
            Directory = items
            Items = items
        }
        |> pushUndo action

    let actual = seqResult (Undo.undo fs progress) model

    let expectedError = MainStatus.ToggleHiddenError (false, [createPath "/c/file", ex], 1)
    let expectedUndo = ToggleHidden (true, [item], true)
    let expected =
        { model with
            UndoStack = expectedUndo :: model.UndoStack.Tail
        }
        |> withNewCancelToken
        |> MainModel.withError expectedError
    actual |> assertAreEqual expected

[<Test>]
let ``undo toggleHidden with partial success sets error message, then undo again retries and merges redo item`` () =
    let fs = FakeFileSystem [
        fileWith (hide true) "file1"
        fileWith (hide true) "file2"
        fileWith (hide true) "file3"
        fileWith (hide true) "hidden"
        file "other"
    ]
    let errorItem1 = createFile "/c/file1"
    let errorItem2 = createFile "/c/file2"
    let successful = createFile "/c/file3"
    let items = fs.ItemsIn "/c"
    let actionItems = items |> List.take 3 |> List.map (hide false)
    let action = ToggleHidden (true, actionItems, false)
    let model =
        { testModel with
            Directory = items
            Items = items |> List.filter (not << _.IsHidden)
            SelectedItems = actionItems
        }
        |> pushUndo action

    let assertErrorModel singleError actual =
        let errorItems = [
            if not singleError then
                errorItem1
            errorItem2
        ]
        let expectedSuccessful =
            [
                successful
                if singleError then
                    errorItem1
            ]
        let expectedItems = [
            createFile "/c/file1" |> hide (not singleError)
            createFile "/c/file2" |> hide true
            createFile "/c/file3"
            createFile "/c/hidden" |> hide true
            createFile "/c/other"
        ]
        let expectedErrorPaths = errorItems |> List.map (fun i -> i.Path, ex)
        let expectedError = MainStatus.ToggleHiddenError (false, expectedErrorPaths, errorItems.Length + 1)
        let expectedUndo = ToggleHidden (true, errorItems, true)
        let expectedRedo = ToggleHidden (true, expectedSuccessful, true)
        let expected =
            { model with
                Directory = expectedItems
                Items = expectedItems |> List.filter (not << _.IsHidden)
                SelectedItems = []
                UndoStack = expectedUndo :: model.UndoStack.Tail
                RedoStack = expectedRedo :: model.RedoStack
                CancelToken = CancelToken()
            }
            |> MainModel.withError expectedError
        actual |> assertAreEqual expected
        fs.ItemsIn "/c" |> assertAreEqual expectedItems

    // part one: handles errors
    fs.AddExnPath true ex errorItem1.Path
    fs.AddExnPath true ex errorItem2.Path
    let modelAfterError =
        model
        |> seqResult (Undo.undo fs progress)
        |>! assertErrorModel false

    // part two: undo should retry and handle error
    fs.AddExnPath true ex errorItem2.Path
    let modelAfterRetry =
        modelAfterError
        |> seqResult (Undo.undo fs progress)
        |>! assertErrorModel true

    // part three: undo with no error should complete the operation and merge redo item
    let actual = modelAfterRetry |> seqResult (Undo.undo fs progress)

    let expectedItems = [
        createFile "/c/file1"
        createFile "/c/file2"
        createFile "/c/file3"
        createFile "/c/hidden" |> hide true
        createFile "/c/other"
    ]
    let expectedSuccessful = [successful; errorItem1; errorItem2]
    let expectedRedo = ToggleHidden (true, expectedSuccessful, false)
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems |> List.filter (not << _.IsHidden)
            SelectedItems = []
            Cursor = expectedItems |> Seq.findIndex (fun i -> i.Name = errorItem2.Name)
            UndoStack = model.UndoStack.Tail
            RedoStack = expectedRedo :: model.RedoStack
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (MainStatus.UndoAction (ToggleHidden (true, [errorItem2], false), 1, 1))
    actual |> assertAreEqual expected
    fs.ItemsIn "/c" |> assertAreEqual expectedItems

[<Test>]
let ``undo toggleHidden sets non-hidden until canceled, then undo resumes and merges redo item`` () =
    let fs = FakeFileSystem [
        fileWith (hide true) "file1"
        fileWith (hide true) "file2"
        fileWith (hide true) "file3"
        fileWith (hide true) "file4"
        fileWith (hide true) "hidden"
        file "other"
    ]
    let items = fs.ItemsIn "/c"
    let actionItems = items |> List.take 4 |> List.map (hide false)
    let action = ToggleHidden (true, actionItems, false)
    let model =
        { testModel with
            Directory = items
            Items = items |> List.filter (not << _.IsHidden)
        }
        |> pushUndo action
    let writesBeforeCancel = 2

    // part one: cancels correctly
    let modelAfterCancel =
        seqResultWithCancelTokenCallback (fs.CancelAfterWriteCount writesBeforeCancel) (Undo.undo fs progress) model

    let resumeItems = actionItems |> List.skip writesBeforeCancel
    (
        let expectedItems = [
            createFile "/c/file1"
            createFile "/c/file2"
            createFile "/c/file3" |> hide true
            createFile "/c/file4" |> hide true
            createFile "/c/hidden" |> hide true
            createFile "/c/other"
        ]
        let expectedUndo = ToggleHidden (true, resumeItems, true)
        let expectedRedo = ToggleHidden (true, actionItems |> List.take writesBeforeCancel, true)
        let expected =
            { model with
                Directory = expectedItems
                Items = expectedItems |> List.filter (not << _.IsHidden)
                SelectedItems = actionItems |> List.take writesBeforeCancel
                UndoStack = expectedUndo :: model.UndoStack.Tail
                RedoStack = expectedRedo :: model.RedoStack
            }
            |> MainModel.withMessage (MainStatus.CancelledToggleHidden (true, true, writesBeforeCancel, actionItems.Length))
        modelAfterCancel |> assertAreEqual expected
        fs.ItemsIn "/c" |> assertAreEqual expectedItems
    )

    // part two: undo should complete the operation and merge redo item
    let actual = modelAfterCancel |> seqResult (Undo.undo fs progress)

    let expectedItems = [
        createFile "/c/file1"
        createFile "/c/file2"
        createFile "/c/file3"
        createFile "/c/file4"
        createFile "/c/hidden" |> hide true
        createFile "/c/other"
    ]
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems |> List.filter (not << _.IsHidden)
            SelectedItems = resumeItems
            Cursor = writesBeforeCancel
            UndoStack = model.UndoStack.Tail
            RedoStack = action :: model.RedoStack
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (MainStatus.UndoAction (ToggleHidden (true, resumeItems, false), 1, 1))
    actual |> assertAreEqual expected
    fs.ItemsIn "/c" |> assertAreEqual expectedItems
