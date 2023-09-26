module Koffee.MainActionTests_Rename

open NUnit.Framework
open FsUnitTyped
open Koffee.Main

// rename tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Rename file calls file sys move and openPath and updates history`` diffCaseOnly =
    let fs = FakeFileSystem [
        file "another"
        file "my file"
        file "nacho file"
    ]
    let item = fs.Item "/c/my file"
    let newPath = if diffCaseOnly then "/c/My File" else "/c/renamed"
    let renamed = createFile newPath
    let items = fs.ItemsIn "/c"
    let model =
        { testModel with
            Directory = items
            Items = items
            Cursor = 1
        } |> withHistoryPaths (historyPaths {
            "/c/unrelated/file"
            item
        })

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
        }.WithMessage (MainStatus.ActionComplete (expectedAction, model.PathFormat))
        |> withHistoryPaths (historyPaths {
            model.History.Paths.[0]
            newPath
        })
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        file "another"
        file renamed.Name
        file "nacho file"
    ]

[<Test>]
let ``Rename folder calls file sys move and updates history`` () =
    let fs = FakeFileSystem [
        folder "another" []
        folder "folder" [
            file "my file"
            file "your file"
        ]
    ]
    let item = fs.Item "/c/folder"
    let newPath = "/c/renamed/"
    let renamed = createFolder newPath
    let items = fs.ItemsIn "/c"
    let model =
        { testModel with
            Directory = items
            Items = items
            Cursor = 1
        } |> withHistoryPaths (historyPaths {
            "/c/folder/your file"
            "/c/unrelated/file"
            item
        })

    let actual = Action.rename fs item renamed.Name model
                 |> assertOk

    let expectedItems = [
        items.[0]
        renamed
    ]
    let expectedAction = RenamedItem (item, renamed.Name)
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems |> sortByPath
            Cursor = 1
            UndoStack = expectedAction :: model.UndoStack
            RedoStack = []
        }.WithMessage (MainStatus.ActionComplete (expectedAction, model.PathFormat))
        |> withHistoryPaths (historyPaths {
            newPath + "your file"
            model.History.Paths.[1]
            newPath
        })
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        folder "another" []
        folder "renamed" [
            file "my file"
            file "your file"
        ]
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
        }.WithMessage (MainStatus.ActionComplete (expectedAction, model.PathFormat))
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

    actual |> shouldEqual (Error (MainStatus.CannotUseNameAlreadyExists ("rename", File, newName, existingHidden)))
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
    actual |> shouldEqual (Error (MainStatus.ItemActionError (expectedAction, model.PathFormat, ex)))

// undo rename tests

[<TestCase(false, false)>]
[<TestCase(true, false)>]
[<TestCase(false, true)>]
[<TestCase(true, true)>]
let ``Undo rename file changes name back to original and updates history`` curPathDifferent diffCaseOnly =
    let currentName = if diffCaseOnly then "File" else "renamed"
    let fs = FakeFileSystem [
        file "another"
        file currentName
    ]
    let previous = createFile "/c/file"
    let current = fs.Item ("/c/" + currentName)
    let location = if curPathDifferent then "/c/other" else "/c"
    let action = RenamedItem (previous, current.Name)
    let model =
        testModel
        |> withLocation location
        |> pushUndo action
        |> withHistoryPaths (historyPaths {
            "/c/unrelated/file"
            current
        })

    let actual = seqResult (Action.undo fs progress) model

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
        }.WithMessage (MainStatus.UndoAction (action, model.PathFormat, 1, 1))
        |> withBackIf curPathDifferent (model.Location, 0)
        |> withHistoryPaths (historyPaths {
            previous.Path.Parent, true
            model.History.Paths.[0]
            previous
        })
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        file "another"
        file previous.Name
    ]

[<Test>]
let ``Undo rename folder changes name back to original and updates history`` () =
    let fs = FakeFileSystem [
        folder "another" []
        folder "renamed" []
    ]
    let previous = createFolder "/c/folder"
    let current = fs.Item "/c/renamed"
    let action = RenamedItem (previous, current.Name)
    let model =
        testModel
        |> pushUndo action
        |> withHistoryPaths (historyPaths {
            current.Path.Join "file", false
            "/c/unrelated/file"
            current
        })

    let actual = seqResult (Action.undo fs progress) model

    let expectedItems = [
        createFolder "/c/another"
        previous
    ]
    let expected =
        { testModel with
            Directory = expectedItems
            Items = expectedItems
            Cursor = 1
            RedoStack = action :: model.RedoStack
        }.WithMessage (MainStatus.UndoAction (action, model.PathFormat, 1, 1))
        |> withHistoryPaths (historyPaths {
            previous.Path.Parent, true
            previous.Path.Join "file", false
            model.History.Paths.[1]
            previous
        })
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        folder "another" []
        folder "folder" []
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

    let actual = seqResult (Action.undo fs progress) model

    let expectedError = MainStatus.CannotUseNameAlreadyExists ("rename", File, previous.Name, existingHidden)
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

    let actual = seqResult (Action.undo fs progress) model

    let expectedError = MainStatus.ItemActionError (RenamedItem (current, previous.Name), model.PathFormat, ex)
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