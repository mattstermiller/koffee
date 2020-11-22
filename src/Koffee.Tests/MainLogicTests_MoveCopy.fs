module Koffee.MainLogicTests_MoveCopy

open System
open NUnit.Framework
open FsUnitTyped

let withReg reg model =
    { model with Config = { model.Config with YankRegister = reg } }

let copyName = MainLogic.Action.getCopyName

let copyNames name num =
    List.init num (copyName name)

let putActionCases () = [
    TestCaseData(Move)
    TestCaseData(Copy)
    TestCaseData(Shortcut)
]

let putItemOverwriteCases () =
    putActionCases () |> List.collect (fun c -> [
        TestCaseData(c.Arguments.[0], false)
        TestCaseData(c.Arguments.[0], true)
    ])

[<TestCaseSource("putItemOverwriteCases")>]
let ``Put item in different folder with item of same name prompts for overwrite`` action existingHidden =
    let destName = if action = Shortcut then "file.lnk" else "file"
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
    let item = Some (src.Path, src.Type, action)
    let model = testModel |> withReg item
    let expectedItems = fs.ItemsIn "/c"

    let actual = seqResult (MainLogic.Action.put fs false) model

    let expected =
        { model with
            Directory = expectedItems |> sortByPath
            Items = expectedItems |> List.filter (fun i -> i.Name <> "hidden")
            Cursor = 2
            InputMode = Some (Confirm (Overwrite (action, src, dest)))
        }
    assertAreEqual expected actual
    fs.ItemsIn "/c" |> shouldEqual expectedItems

[<Test>]
let ``Put handles missing register item`` () =
    let src = createFile "/c/file"
    let fs = FakeFileSystem []
    let model = testModel |> withReg (Some (src.Path, src.Type, Move))

    let actual = seqResult (MainLogic.Action.put fs false) model

    let expected = model.WithError (YankRegisterItemMissing (src.Path.Format model.PathFormat))
    assertAreEqual expected actual

[<Test>]
let ``Put handles error reading register item`` () =
    let src = createFile "/c/file"
    let fs = FakeFileSystem []
    fs.AddExn ex (string src.Path)
    let model = testModel |> withReg (Some (src.Path, src.Type, Move))

    let actual = seqResult (MainLogic.Action.put fs false) model

    let expected = model.WithError (ActionError ("read yank register item", ex))
    assertAreEqual expected actual

[<TestCaseSource("putActionCases")>]
let ``Put item handles file system errors`` action =
    let fs = FakeFileSystem [
        folder "folder" [
            file "file"
        ]
    ]
    let src = fs.Item "/c/folder/file"
    let destPath = "/c/file" + (if action = Shortcut then ".lnk" else "")
    fs.AddExn ex destPath
    let item = Some (src.Path, src.Type, action)
    let model = testModel |> withReg item
    let expectedFs = fs.Items

    let actual = seqResult (MainLogic.Action.put fs false) model

    let expectedAction = PutItem (action, src, createPath destPath)
    let expected = model.WithError (ItemActionError (expectedAction, model.PathFormat, ex))
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
    let putAction = if copy then Copy else Move
    let model = testModel |> withReg (Some (src.Path, src.Type, putAction))

    let actual = seqResult (MainLogic.Action.put fs overwrite) model

    let expectedAction = PutItem (putAction, src, dest.Path)
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
            Status = Some <| MainStatus.actionComplete expectedAction testModel.PathFormat
        }
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        folder "folder" [
            if copy then
                fileWith (size 41L) "file"
        ]
        fileWith (size 41L) "file"
    ]

// move tests

[<Test>]
let ``Put item to move in same folder returns error``() =
    let fs = FakeFileSystem [
        file "file"
    ]
    let src = fs.Item "/c/file"
    let model = testModel |> withReg (Some (src.Path, src.Type, Move))
    let expectedFs = fs.Items

    let actual = seqResult (MainLogic.Action.put fs false) model

    let expected = model.WithError CannotMoveToSameFolder
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
    let action = PutItem (Move, original, moved.Path)
    let location = if curPathDifferent then "/c/other" else "/c"
    let model = testModel |> withLocation location |> pushUndo action

    let actual = seqResult (MainLogic.Action.undo fs) model

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
            RedoStack = action :: testModel.RedoStack
            Status = Some (MainStatus.undoAction action testModel.PathFormat)
        }
        |> withLocation "/c"
        |> withBackIf curPathDifferent (model.Location, 0)
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        folder "dest" []
        folder "other" []
        file "another"
        file "file"
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
    let action = PutItem (Move, original, moved.Path)
    let model = testModel |> withLocation "/c/other" |> pushUndo action
    let expectedFs = fs.Items

    let actual = seqResult (MainLogic.Action.undo fs) model

    let expected = model.WithError (CannotUndoMoveToExisting original) |> popUndo
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
    fs.AddExn ex "/c/file"
    let action = PutItem (Move, original, moved.Path)
    let model = testModel |> withLocation "/c/other" |> pushUndo action
    let expectedFs = fs.Items

    let actual = seqResult (MainLogic.Action.undo fs) model

    let expectedAction = PutItem (Move, moved, original.Path)
    let expected = model.WithError (ItemActionError (expectedAction, model.PathFormat, ex)) |> popUndo
    assertAreEqual expected actual
    fs.Items |> shouldEqual expectedFs

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

    let actual = seqResult (MainLogic.Action.put fs false) model

    let expectedItems =
        [
            createFile "/c/file"
            yield! copyNames "file" (existingCopies+1) |> List.map (fun name -> createFile ("/c/" + name))
        ] |> sortByPath
    let expectedPath = createPath ("/c/" + copyName "file" existingCopies)
    let expectedAction = PutItem (Copy, createFile "/c/file", expectedPath)
    let expected =
        { testModel with
            Directory = expectedItems
            Items = expectedItems
            Cursor = expectedItems |> List.findIndex (fun i -> i.Path = expectedPath)
            UndoStack = expectedAction :: testModel.UndoStack
            RedoStack = []
            Status = Some <| MainStatus.actionComplete expectedAction model.PathFormat
        }
    assertAreEqual expected actual

// undo copy tests

[<TestCase(false, true)>]
[<TestCase(false, false)>]
[<TestCase(false, null)>]
[<TestCase(true, true)>]
[<TestCase(true, false)>]
[<TestCase(true, null)>]
let ``Undo copy item deletes when it has the same timestamp, recycles otherwise`` curPathDifferent (sameTimestamp: bool Nullable) =
    let time = DateTime(2000, 1, 1)
    let sameTimestamp = sameTimestamp |> Option.ofNullable
    let copyTime = sameTimestamp |> Option.map (fun same -> if same then time else time.AddDays(1.0))
    let fs = FakeFileSystem [
        folder "other" []
        folder "src" [
            fileWith (modified time) "file"
        ]
        fileWith (modifiedOpt copyTime >> size 1L) "file"
    ]
    let original = fs.Item "/c/src/file"
    let copied = fs.Item "/c/file"
    let action = PutItem (Copy, original, copied.Path)
    let location = if curPathDifferent then "/c/other" else "/c"
    let model = testModel |> withLocation location |> pushUndo action

    let actual = seqResult (MainLogic.Action.undo fs) model

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
            Status = Some (MainStatus.undoAction action model.PathFormat)
        }
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        folder "other" []
        folder "src" [
            fileWith (modified time) "file"
        ]
    ]
    fs.RecycleBin |> shouldEqual [
        if sameTimestamp <> Some true then
            copied
    ]

[<Test>]
let ``Undo copy item handles errors by returning error and consuming action`` () =
    let fs = FakeFileSystem [
        folder "src" [
            file "file"
        ]
        file "file"
    ]
    let original = fs.Item "/c/src/file"
    let copied = fs.Item "/c/file"
    fs.AddExnPath (exn "GetItem error") copied.Path
    fs.AddExnPath ex copied.Path
    let model = testModel
    let expectedFs = fs.Items

    let actual = seqResult (MainLogic.Action.undoCopy fs original copied.Path) model

    let action = DeletedItem (copied, false)
    let expected = model.WithError (ItemActionError (action, model.PathFormat, ex))
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

    let actual = seqResult (MainLogic.Action.put fs overwrite) model

    let expectedAction = PutItem (Shortcut, target, shortcut.Path)
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
            Status = Some <| MainStatus.actionComplete expectedAction model.PathFormat
        }
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
    let model = testModel |> withLocation location

    let actual = MainLogic.Action.undoShortcut fs shortcut.Path model
                 |> assertOk

    let expectedItems = [
        createFolder "/c/other"
    ]
    let expected =
        if curPathDifferent then
            model
        else
            { model with
                Directory = expectedItems |> sortByPath
                Items = expectedItems
                Cursor = 0
            }
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
    fs.AddExnPath ex shortcut.Path
    let model = testModel

    let actual = MainLogic.Action.undoShortcut fs shortcut.Path model

    let action = DeletedItem (shortcut, true)
    let expected = Error (ItemActionError (action, model.PathFormat, ex))
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        file "file.lnk"
    ]
