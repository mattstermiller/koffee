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
    assertAreEqual expected actual
    fs.ItemsIn "/c" |> shouldEqual expectedItems

[<Test>]
let ``Put handles missing register item`` () =
    let src = createFile "/c/file"
    let fs = FakeFileSystem []
    let model = testModel |> withReg (Some (src.Path, src.Type, Move))

    let actual = seqResult (Action.put fs progress false) model

    let expected = model.WithError (YankRegisterItemMissing (src.Path.Format model.PathFormat))
    assertAreEqual expected actual

[<Test>]
let ``Put handles error reading register item`` () =
    let src = createFile "/c/file"
    let fs = FakeFileSystem []
    fs.AddExnPath false ex src.Path
    let model = testModel |> withReg (Some (src.Path, src.Type, Move))

    let actual = seqResult (Action.put fs progress false) model

    let expected = model.WithError (ActionError ("read yank register item", ex))
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

    let expected = model.WithError (PutError (false, putType, [(src, ex)], 1))
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
    let model = testModel |> withReg (Some (src.Path, src.Type, putType))

    let actual = seqResult (Action.put fs progress overwrite) model

    let putItem = { Item = src; Dest = dest.Path; DestExists = overwrite }
    let expectedAction = PutItems (putType, putItem, [putItem])
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

[<Test>]
let ``Put item to move in same folder returns error``() =
    let fs = FakeFileSystem [
        file "file"
    ]
    let src = fs.Item "/c/file"
    let model = testModel |> withReg (Some (src.Path, src.Type, Move))
    let expectedFs = fs.Items

    let actual = seqResult (Action.put fs progress false) model

    let expected = model.WithError CannotMoveToSameFolder
    assertAreEqual expected actual
    fs.Items |> shouldEqual expectedFs

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
            ]
        ]
    ]
    let src = fs.Item "/c/fruit"
    let dest = createFolder "/c/dest/fruit"
    let putType = if copy then Copy else Move
    let model = testModel |> withLocation "/c/dest" |> withReg (Some (src.Path, src.Type, putType))

    let actual = seqResult (Action.put fs progress true) model

    let intent = { Item = src; Dest = dest.Path; DestExists = true }
    let createPutItem item destExists = {
        Item = item
        Dest = item.Path.Format Unix |> String.replace "/c/" "/c/dest/" |> createPath
        DestExists = destExists
    }
    let expectedPut = [
        if copy then
            createPutItem (createFile "/c/fruit/bushes/blueberry") false
        else
            createPutItem (createFolder "/c/fruit/bushes") false
        createPutItem (createFile "/c/fruit/tomato") false
        createPutItem (createFile "/c/fruit/trees/apple" |> size 7L) true
        createPutItem (createFile "/c/fruit/trees/banana") false
    ]
    let expectedAction = PutItems (putType, intent, expectedPut)
    let expectedItems = [dest]
    let expected =
        { testModel with
            Directory = expectedItems |> sortByPath
            Items = expectedItems
            UndoStack = expectedAction :: testModel.UndoStack
            RedoStack = []
            Status = Some <| MainStatus.actionComplete expectedAction testModel.PathFormat
        } |> withLocation "/c/dest"
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
                file "tomato"
            ]
        ]
    ]

// move tests

[<TestCase(false, false)>]
[<TestCase(true, false)>]
[<TestCase(true, true)>]
let ``Put folder to move deletes source folder after enumerated move`` enumerated deleteError =
    let fs = FakeFileSystem [
        folder "dest" [
            if enumerated then
                folder "src" []
        ]
        folder "src" [
            folder "folder" []
            file "file"
        ]
    ]
    let src = fs.Item "/c/src"
    let dest = fs.Item "/c/dest"
    if enumerated && deleteError then
        fs.AddExnPath true ex src.Path
    let model = testModel |> withLocation "/c/dest" |> withReg (Some (src.Path, src.Type, Move))

    let actual = seqResult (Action.put fs progress enumerated) model

    let intent = { Item = src; Dest = dest.Path.Join src.Name; DestExists = enumerated }
    let expectedPut =
        if enumerated then
            [
                { Item = createFile "/c/src/file"; Dest = createPath "/c/dest/src/file"; DestExists = false }
                { Item = createFolder "/c/src/folder"; Dest = createPath "/c/dest/src/folder"; DestExists = false }
            ]
        else
            [intent]
    let expectedAction = PutItems (Move, intent, expectedPut)
    let expectedItems = [createFolder "/c/dest/src"]
    let expected =
        { testModel.WithLocation dest.Path with
            Directory = expectedItems |> sortByPath
            Items = expectedItems
            UndoStack = expectedAction :: testModel.UndoStack
            RedoStack = []
            Status = Some <| MainStatus.actionComplete expectedAction testModel.PathFormat
        } |> fun m ->
            if enumerated && deleteError then
                m.WithError (CouldNotDeleteMoveSource (src.Name, ex))
            else m
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        folder "dest" [
            folder "src" [
                folder "folder" []
                file "file"
            ]
        ]
        if enumerated && deleteError then
            folder "src" []
    ]

[<Test>]
let ``Put folder to move handles partial success by updating undo and setting error message``() =
    let fs = FakeFileSystem [
        folder "dest" [
            folder "other" []
            folder "src" []
        ]
        folder "src" [
            file "file"
            file "unmovable"
        ]
    ]
    let errorItem = createFile "/c/src/unmovable"
    fs.AddExnPath true ex errorItem.Path
    let src = fs.Item "/c/src"
    let dest = fs.Item "/c/dest"
    let model = testModel |> withLocation "/c/dest" |> withReg (Some (src.Path, src.Type, Move))

    let actual = seqResult (Action.put fs progress true) model

    let intent = { Item = src; Dest = dest.Path.Join src.Name; DestExists = true }
    let expectedPut = [{ Item = createFile "/c/src/file"; Dest = createPath "/c/dest/src/file"; DestExists = false }]
    let expectedAction = PutItems (Move, intent, expectedPut)
    let expectedItems = [
        createFolder "/c/dest/other"
        createFolder "/c/dest/src"
    ]
    let expectedError = PutError (false, Move, [(errorItem, ex)], 2)
    let expected =
        { testModel.WithLocation(dest.Path).WithError(expectedError) with
            Directory = expectedItems |> sortByPath
            Items = expectedItems
            Cursor = 1
            UndoStack = expectedAction :: testModel.UndoStack
            RedoStack = []
        }
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        folder "dest" [
            folder "other" []
            folder "src" [
                file "file"
            ]
        ]
        folder "src" [
            file "unmovable"
        ]
    ]

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
    let action = PutItems (Move, putItem, [putItem])
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
            Status = Some (MainStatus.undoAction action model.PathFormat 1)
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
    let actualMoved = [
        { Item = createFile "/d/moved/file"; Dest = moved.Path.Join "file"; DestExists = false }
        { Item = createFile "/d/moved/other"; Dest = moved.Path.Join "other"; DestExists = false }
    ]
    let action = PutItems (Move, putItem, actualMoved)
    let model = testModel |> pushUndo action

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
            Status = Some (MainStatus.undoAction action testModel.PathFormat 1)
        }
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
let ``Undo move copies back items that were overwrites`` () =
    let fs = FakeFileSystem [
        folder "dest" [
            folder "moved" [
                file "file"
                file "other"
            ]
        ]
    ]
    let moved = fs.Item "/c/dest/moved"
    let original = createFolder "/c/moved"
    let putItem = { Item = original; Dest = moved.Path; DestExists = true }
    let actualMoved = [
        { Item = createFile "/c/moved/file"; Dest = moved.Path.Join "file"; DestExists = true }
        { Item = createFile "/c/moved/other"; Dest = moved.Path.Join "other"; DestExists = false }
    ]
    let action = PutItems (Move, putItem, actualMoved)
    let model = testModel |> pushUndo action

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
            Status = Some (MainStatus.undoAction action testModel.PathFormat 1)
        }
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        folder "dest" [
            folder "moved" [
                file "file"
            ]
        ]
        folder "moved" [
            file "file"
            file "other"
        ]
    ]

[<TestCase(false, false)>]
[<TestCase(false, true)>]
[<TestCase(true, false)>]
[<TestCase(true, true)>]
let ``Undo move handles partial success by updating undo and setting error message`` errorCausedByExisting destExisted =
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
    let original = createFolder "/c/moved"
    let destPath = createPath "/c/dest/moved"
    let putItem = { Item = original; Dest = destPath; DestExists = destExisted }
    let actualMoved = [
        { Item = createFile "/c/moved/file"; Dest = destPath.Join "file"; DestExists = false }
        { Item = createFile "/c/moved/other"; Dest = destPath.Join "other"; DestExists = false }
    ]
    let action = PutItems (Move, putItem, actualMoved)
    let model = testModel |> pushUndo action

    let actual = seqResult (Action.undo fs progress) model

    let expectedItems = [
        createFolder "/c/dest"
        createFolder "/c/moved"
    ]
    let expectedExn = if errorCausedByExisting then exn ErrorMessages.undoMoveBlockedByExisting else ex
    let expectedError = PutError (true, Move, [(errorItem, expectedExn)], 2)
    // DestExists should be set to true on intent so that redo would merge, otherwise it would always return error
    let expectedAction = PutItems (Move, { putItem with DestExists = true }, actualMoved |> List.skip 1)
    let expected =
        { model.WithError(expectedError) with
            Directory = expectedItems |> sortByPath
            Items = expectedItems
            Cursor = 1
            UndoStack = model.UndoStack.Tail
            RedoStack = expectedAction :: testModel.RedoStack
        }
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
    let action = PutItems (Move, putItem, [putItem])
    let model = testModel |> withLocation "/c/other" |> pushUndo action
    let expectedFs = fs.Items

    let actual = seqResult (Action.undo fs progress) model

    let expectedError = PutError (true, Move, [(original, exn ErrorMessages.undoMoveBlockedByExisting)], 1)
    let expected = model.WithError expectedError |> popUndo
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
    let action = PutItems (Move, putItem, [putItem])
    let model = testModel |> withLocation "/c/other" |> pushUndo action
    let expectedFs = fs.Items

    let actual = seqResult (Action.undo fs progress) model

    let expectedError = PutError (true, Move, [(moved, ex)], 1)
    let expected = model.WithError expectedError |> popUndo
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
    let item = createFolder "/c/moved"
    let destPath = createPath "/c/dest/moved"
    let putItem = { Item = item; Dest = destPath; DestExists = true }
    let actualMoved = [
        // even if this file was not originally an overwrite, a redo should overwrite since the we're redoing the intent
        // to merge the folder
        { Item = createFile "/c/moved/file" |> size 2L; Dest = destPath.Join "file"; DestExists = false }
        { Item = createFile "/c/moved/other"; Dest = destPath.Join "other"; DestExists = false }
    ]
    let action = PutItems (Move, putItem, actualMoved)
    let model = testModel |> pushRedo action
    let expectedFile = fs.Item "/c/moved/file"

    let actual = seqResult (Action.redo fs progress) model

    let expectedItems = [
        createFolder "/c/dest/another"
        createFolder "/c/dest/moved"
    ]
    let expectedActual = [
        { Item = createFile "/c/moved/file" |> size 7L; Dest = destPath.Join "file"; DestExists = true }
        actualMoved.[1]
    ]
    let expectedAction = PutItems (Move, putItem, expectedActual)
    let expected =
        { model.WithPushedLocation(destPath.Parent) with
            Directory = expectedItems |> sortByPath
            Items = expectedItems
            Cursor = 1
            UndoStack = expectedAction :: model.UndoStack
            RedoStack = model.RedoStack.Tail
            Status = Some (MainStatus.redoAction expectedAction model.PathFormat 1)
        }
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

[<Test>]
let ``Redo move performs move of intent instead of actual`` () =
    // simulate new file created externally in source folder
    let fs = FakeFileSystem [
        drive 'c' [
            folder "dest" [
                folder "another" []
            ]
        ]
        drive 'd' [
            folder "moved" [
                file "file"
                file "new"
            ]
        ]
    ]
    let item = createFolder "/d/moved"
    let destPath = createPath "/c/dest/moved"
    let putItem = { Item = item; Dest = destPath; DestExists = false }
    let actualMoved = [
        { Item = createFile "/d/moved/file"; Dest = destPath.Join "file"; DestExists = false }
    ]
    let action = PutItems (Move, putItem, actualMoved)
    let model = testModel |> pushRedo action

    let actual = seqResult (Action.redo fs progress) model

    let expectedActual = actualMoved @ [
        { Item = createFile "/d/moved/new"; Dest = destPath.Join "new"; DestExists = false }
    ]
    let expectedAction = PutItems (Move, putItem, expectedActual)
    let expectedItems = [
        createFolder "/c/dest/another"
        createFolder "/c/dest/moved"
    ]
    let expected =
        { model.WithPushedLocation destPath.Parent with
            Directory = expectedItems |> sortByPath
            Items = expectedItems
            Cursor = 1
            UndoStack = expectedAction :: model.UndoStack
            RedoStack = model.RedoStack.Tail
            Status = Some (MainStatus.redoAction expectedAction testModel.PathFormat 1)
        }
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        drive 'c' [
            folder "dest" [
                folder "another" []
                folder "moved" [
                    file "file"
                    file "new"
                ]
            ]
        ]
        drive 'd' []
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
    let expectedAction = PutItems (Copy, putItem, [putItem])
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
let ``Undo copy file deletes when it has the same timestamp or recycles otherwise`` curPathDifferent (sameTimestamp: bool Nullable) =
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
    let putItem = { Item = original; Dest = copied.Path; DestExists = false }
    let action = PutItems (Copy, putItem, [putItem])
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
            Status = Some (MainStatus.undoAction action model.PathFormat 1)
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

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo copy folder deletes items that were copied and removes dest folders if empty`` hasNewItem =
    let fs = FakeFileSystem [
        folder "copied" [
            folder "folder" [
                file "sub"
            ]
            file "file"
            file "other"
        ]
        folder "dest" [
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
    let copied = fs.Item "/c/dest/copied"
    let original = createFolder "/c/copied"
    let putItem = { Item = original; Dest = copied.Path; DestExists = false }
    let actualCopied = [
        { Item = createFolder "/c/copied/folder"; Dest = copied.Path.Join "folder"; DestExists = false }
        { Item = createFile "/c/copied/file"; Dest = copied.Path.Join "file"; DestExists = false }
        { Item = createFile "/c/copied/other"; Dest = copied.Path.Join "other"; DestExists = false }
    ]
    let action = PutItems (Copy, putItem, actualCopied)
    let model = testModel |> pushUndo action

    let actual = seqResult (Action.undo fs progress) model

    let expected =
        { model with
            UndoStack = model.UndoStack.Tail
            RedoStack = action :: testModel.RedoStack
            Status = Some (MainStatus.undoAction action testModel.PathFormat 1)
        }
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        folder "copied" [
            folder "folder" [
                file "sub"
            ]
            file "file"
            file "other"
        ]
        folder "dest" [
            if hasNewItem then
                folder "copied" [
                    file "new"
                ]
        ]
    ]

[<Test>]
let ``Undo copy does nothing for items that were overwrites`` () =
    let fs = FakeFileSystem [
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
    let copied = fs.Item "/c/dest/copied"
    let original = createFolder "/c/copied"
    let putItem = { Item = original; Dest = copied.Path; DestExists = true }
    let actualCopied = [
        { Item = createFile "/c/copied/file"; Dest = copied.Path.Join "file"; DestExists = true }
        { Item = createFile "/c/copied/other"; Dest = copied.Path.Join "other"; DestExists = false }
    ]
    let action = PutItems (Copy, putItem, actualCopied)
    let model = testModel |> pushUndo action

    let actual = seqResult (Action.undo fs progress) model

    let expected =
        { model with
            UndoStack = model.UndoStack.Tail
            RedoStack = action :: testModel.RedoStack
            Status = Some (MainStatus.undoAction action testModel.PathFormat 1)
        }
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
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

[<TestCase(false)>] // error is during deleting an item
[<TestCase(true)>] // error is during deleting empty original destination folder
let ``Undo copy handles partial success by updating undo and setting error message`` errorIsDeleteDest =
    let fs = FakeFileSystem [
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
    let copied = fs.Item "/c/dest/copied"
    let original = createFolder "/c/copied"
    let errorItem =
        if errorIsDeleteDest
        then copied
        else fs.Item "/c/dest/copied/file"
    fs.AddExnPath true ex errorItem.Path
    let putItem = { Item = original; Dest = copied.Path; DestExists = false }
    let actualCopied = [
        { Item = createFile "/c/copied/file"; Dest = copied.Path.Join "file"; DestExists = false }
        { Item = createFile "/c/copied/other"; Dest = copied.Path.Join "other"; DestExists = false }
    ]
    let action = PutItems (Copy, putItem, actualCopied)
    let model = testModel |> pushUndo action

    let actual = seqResult (Action.undo fs progress) model

    let expectedError =
        if errorIsDeleteDest
        then CouldNotDeleteCopyDest (copied.Name, ex)
        else UndoCopyError ([errorItem, ex], 2)
    let expectedAction = PutItems (Copy, putItem, actualCopied |> List.filter (fun pi -> pi.Dest <> errorItem.Path))
    let expected =
        { model.WithError(expectedError) with
            UndoStack = model.UndoStack.Tail
            RedoStack = expectedAction :: testModel.RedoStack
        }
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
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
    let putItem = { Item = original; Dest = copied.Path; DestExists = false }
    fs.AddExnPath false (exn "GetItem error") copied.Path
    fs.AddExnPath false ex copied.Path
    let action = PutItems (Copy, putItem, [putItem])
    let model = testModel |> pushUndo action
    let expectedFs = fs.Items

    let actual = seqResult (Action.undo fs progress) model

    let expected = model.WithError (UndoCopyError ([copied, ex], 1)) |> popUndo
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
    let expectedAction = PutItems (Shortcut, expectedPutItem, [expectedPutItem])
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

    let actual = Action.undoShortcut fs shortcut.Path model
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
    fs.AddExnPath false ex shortcut.Path
    let model = testModel

    let actual = Action.undoShortcut fs shortcut.Path model

    let action = DeletedItem (shortcut, true)
    let expected = Error (ItemActionError (action, model.PathFormat, ex))
    assertAreEqual expected actual
    fs.ItemsShouldEqual [
        file "file.lnk"
    ]

/// redo put item tests

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
    let item = createFolder "/c/put"
    let destPath = createPath ("/c/dest/put" + if putType = Shortcut then ".lnk" else "")
    let putItem = { Item = item; Dest = destPath; DestExists = false }
    let actualPut = [
        if putType = Shortcut then
            putItem
        else
            { Item = createFile "/c/put/file"; Dest = destPath.Join "file"; DestExists = false }
            { Item = createFile "/c/put/other"; Dest = destPath.Join "other"; DestExists = false }
    ]
    let action = PutItems (putType, putItem, actualPut)
    let model = testModel |> pushRedo action
    let expectedFs = fs.Items

    let actual = seqResult (Action.redo fs progress) model

    let expectedError = CannotRedoPutToExisting (putType, item, destPath.Format model.PathFormat)
    let expectedItems = [
        createFolder "/c/dest/other"
        if putType = Shortcut then
            createFile "/c/dest/put.lnk"
        else
            createFolder "/c/dest/put"
    ]
    let expected =
        { model.WithPushedLocation(destPath.Parent).WithError(expectedError) with
            Directory = expectedItems |> sortByPath
            Items = expectedItems
            Cursor = 1
            RedoStack = model.RedoStack.Tail
        }
    assertAreEqual expected actual
    fs.Items |> shouldEqual expectedFs
