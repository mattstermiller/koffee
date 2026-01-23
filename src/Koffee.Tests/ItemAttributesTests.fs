module Koffee.ItemAttributesTests

open NUnit.Framework
open FsUnitTyped
open Acadian.FSharp
open Koffee.ItemActionCommands

[<TestCase(false, false)>]
[<TestCase(false, true)>]
// TODO
// [<TestCase(true, false)>]
// [<TestCase(true, true)>]
let ``toggleHidden and redo on non-hidden items sets them to hidden`` isRedo showHidden =
    let fs = FakeFileSystem [
        file "file1"
        file "file2"
        file "file3"
        file "other"
    ]
    let items = fs.ItemsIn "/c"
    let actionItems = items |> List.take 3
    let action = ToggleHidden (true, actionItems, false)
    let model =
        { testModel with
            Directory = items
            Items = items
            SelectedItems = actionItems
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
        createFile "/c/other"
    ]
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            SelectedItems = if showHidden then actionItems else []
            UndoStack = action :: model.UndoStack
            RedoStack = if isRedo then model.RedoStack.Tail else []
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (MainStatus.ActionComplete action)
    actual |> assertAreEqual expected
    fs.ItemsShouldEqualList expectedItems

[<Test>]
let ``toggleHidden on mixed hidden items sets non-hidden ones to hidden`` () =
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
    let expectedAction = ToggleHidden (true, [createFile "/c/file2"], false)
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            SelectedItems = actionItems
            UndoStack = expectedAction :: model.UndoStack
            RedoStack = []
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (MainStatus.ActionComplete expectedAction)
    actual |> assertAreEqual expected
    fs.ItemsShouldEqualList expectedItems

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
    let model =
        { testModel with
            Directory = items
            Items = items
            SelectedItems = actionItems
            MainModel.Config.ShowHidden = true
        }
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
    let expectedAction = ToggleHidden (false, actionItems, false)
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            SelectedItems = actionItems
            UndoStack = expectedAction :: model.UndoStack
            RedoStack = if isRedo then model.RedoStack.Tail else []
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (MainStatus.ActionComplete expectedAction)
    actual |> assertAreEqual expected
    fs.ItemsShouldEqualList expectedItems

[<Test>]
let ``toggleHidden handles error by setting error status`` () =
    let fs = FakeFileSystem [
        file "file"
        file "other"
    ]
    fs.AddExn true ex "/c/file"
    let items = fs.ItemsIn "/c"
    let model =
        { testModel with
            Directory = items
            Items = items
            Cursor = 0
        }

    let actual = seqResult (Attributes.toggleHidden fs progress) model

    let expectedError = MainStatus.ToggleHiddenError (true, [createPath "/c/file", ex], 1)
    let expected =
        model
        |> withNewCancelToken
        |> MainModel.withError expectedError
    actual |> assertAreEqual expected

[<TestCase(false)>]
// TODO
// [<TestCase(true)>]
let ``toggleHidden and redo handles partial success by updating undo and redo and setting error message`` isRedo =
    let fs = FakeFileSystem [
        file "file1"
        file "file2"
        file "file3"
        file "other"
    ]
    let errorItem = fs.Item (createPath "/c/file2")
    fs.AddExnPath true ex errorItem.Path
    let items = fs.ItemsIn "/c"
    let model =
        { testModel with
            Directory = items
            Items = items
            SelectedItems = items |> List.take 3
        }
    let testFunc =
        if isRedo
        then Undo.redo fs progress
        else Attributes.toggleHidden fs progress

    let actual = seqResult testFunc model

    let expectedItems = [
        createFile "/c/file1" |> hide true
        createFile "/c/file2"
        createFile "/c/file3" |> hide true
        createFile "/c/other"
    ]
    let expectedError = MainStatus.ToggleHiddenError (true, [createPath "/c/file2", ex], model.SelectedItems.Length)
    let expectedAction = ToggleHidden (true, model.SelectedItems |> List.except [errorItem], false)
    let expectedRedo =
        ToggleHidden (true, [errorItem], true)
        :: (if isRedo then model.RedoStack.Tail else [])
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems |> List.filter (not << _.IsHidden)
            SelectedItems = [errorItem]
            UndoStack = expectedAction :: model.UndoStack
            RedoStack = expectedRedo
            CancelToken = CancelToken()
        }
        |> MainModel.withError expectedError
    actual |> assertAreEqual expected
    fs.ItemsShouldEqualList expectedItems

[<TestCase(false)>]
// TODO
// [<TestCase(true)>]
let ``toggleHidden and redo sets attribute until canceled, then redo resumes and merges undo item`` isRedo =
    let fs = FakeFileSystem [
        file "file1"
        file "file2"
        file "file3"
        file "file4"
        file "other"
    ]
    let items = fs.ItemsIn "/c"
    let model =
        { testModel with
            Directory = items
            Items = items
            SelectedItems = items |> List.take 4
        }
    let writesBeforeCancel = 2
    let testFunc =
        if isRedo
        then Undo.redo fs progress
        else Attributes.toggleHidden fs progress

    // part one: cancels correctly
    let modelAfterCancel = seqResultWithCancelTokenCallback (fs.CancelAfterWriteCount writesBeforeCancel) testFunc model

    (
        let expectedItems = [
            createFile "/c/file1" |> hide true
            createFile "/c/file2" |> hide true
            createFile "/c/file3"
            createFile "/c/file4"
            createFile "/c/other"
        ]
        let expectedAction = ToggleHidden (true, model.SelectedItems |> List.take writesBeforeCancel, true)
        let expectedRedo =
            ToggleHidden (true, model.SelectedItems |> List.skip writesBeforeCancel, true)
            :: (if isRedo then model.RedoStack.Tail else [])
        let expected =
            { model with
                Directory = expectedItems
                Items = expectedItems |> List.filter (not << _.IsHidden)
                SelectedItems = model.SelectedItems |> List.skip writesBeforeCancel // remaining items should be selected
                UndoStack = expectedAction :: model.UndoStack
                RedoStack = expectedRedo
            }
            |> MainModel.withMessage (MainStatus.ActionComplete expectedAction)
        modelAfterCancel |> assertAreEqual expected
        fs.ItemsShouldEqualList expectedItems
    )

    // part two: redo should complete the operation and merge undo item
    let actual = modelAfterCancel |> seqResult testFunc
    let expectedItems = [
        createFile "/c/file1" |> hide true
        createFile "/c/file2" |> hide true
        createFile "/c/file3" |> hide true
        createFile "/c/file4" |> hide true
        createFile "/c/other"
    ]
    let expectedAction = ToggleHidden (true, model.SelectedItems, false)
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems |> List.filter (not << _.IsHidden)
            SelectedItems = model.SelectedItems
            UndoStack = expectedAction :: model.UndoStack
            RedoStack = if isRedo then model.RedoStack.Tail else []
            CancelToken = CancelToken()
        }
        |> MainModel.withMessage (MainStatus.ActionComplete expectedAction)
    actual |> assertAreEqual expected
    fs.ItemsShouldEqualList expectedItems

// [<TestCase(false)>]
// [<TestCase(true)>]
let ``undo toggleHidden on non-hidden items sets them back to non-hidden`` showHidden =
    let fs = FakeFileSystem [
        fileWith (hide true) "file1"
        fileWith (hide true) "file2"
        fileWith (hide true) "file3"
        file "other"
    ]
    let items = fs.ItemsIn "/c"
    let actionItems = [
        createFile "/c/file1"
        createFile "/c/file2"
        createFile "/c/file3"
    ]
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
        createFile "/c/other"
    ]
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            SelectedItems = if showHidden then actionItems else []
            CancelToken = CancelToken()
        }
        |> popUndo
        |> pushRedo action
        |> MainModel.withMessage (MainStatus.UndoAction (action, 1, 1))
    actual |> assertAreEqual expected
    fs.ItemsShouldEqualList expectedItems

// [<TestCase(false)>]
// [<TestCase(true)>]
let ``undo toggleHidden on hidden items sets them back to hidden`` showHidden =
    let fs = FakeFileSystem [
        file "file1"
        file "file2"
        file "file3"
        file "other"
    ]
    let items = fs.ItemsIn "/c"
    let actionItems = [
        createFile "/c/file1" |> hide true
        createFile "/c/file2" |> hide true
        createFile "/c/file3" |> hide true
    ]
    let action = ToggleHidden (false, actionItems, false)
    let model =
        { testModel with
            Directory = items
            Items = items
            SelectedItems = actionItems
            MainModel.Config.ShowHidden = showHidden
        }
        |> pushUndo action

    let actual = seqResult (Undo.undo fs progress) model

    let expectedItems = [
        yield! actionItems
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
    fs.ItemsShouldEqualList expectedItems

// undo with partial success
// undo cancel and resume
