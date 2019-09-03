module Koffee.MainLogicTests_MoveCopy

open System
open NUnit.Framework
open FsUnitTyped

let itemSameFolder = createItem "/c/path/file 2"
let itemSameFolderWith ext = createItem ("/c/path/file 2" + ext)
let itemDiffFolder = createItem "/c/other/file 2"

let oldItems = [
    createItem "/c/path/file 1"
    createItem "/c/path/file 3"
]

let newItems = [
    createItem "/c/path/file 1"
    itemSameFolder
]

let itemCopy num =
    createItem ("/c/path/" + (MainLogic.Action.getCopyName "file 2" num))

let modelPathItem = createItem "/c/path"

let testModel =
    { baseModel.WithLocation modelPathItem.Path with
        Items = oldItems
        Cursor = 0
    }

let withReg reg model =
    { model with Config = { model.Config with YankRegister = reg } }

let mockGetItemFunc itemFunc path =
    if path = modelPathItem.Path then Ok (Some modelPathItem)
    else Ok (itemFunc path)

let mockGetItem items =
    mockGetItemFunc (fun path -> items |> List.tryFind (fun n -> n.Path = path))

let ex = UnauthorizedAccessException() :> exn

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
    let src = itemDiffFolder
    let dest = { itemSameFolderWith (if action = Shortcut then ".lnk" else "") with IsHidden = existingHidden }
    let newItems = [newItems.[0]; dest]
    let newDir = newItems @ [{ oldItems.[1] with IsHidden = true }]
    let fsReader = FakeFileSystemReader()
    fsReader.GetItem <- mockGetItem [src; dest]
    fsReader.GetItems <- fun _ -> Ok newDir
    let fsWriter = FakeFileSystemWriter()
    let item = Some (src.Path, src.Type, action)
    let model = testModel |> withReg item

    let actual = seqResult (MainLogic.Action.put fsReader fsWriter false) model

    let expected =
        { (testModel |> withReg item) with
            Directory = newDir
            Items = newItems
            Cursor = 1
            InputMode = Some (Confirm (Overwrite (action, src, dest)))
        }
    assertAreEqual expected actual

[<Test>]
let ``Put handles missing register item`` () =
    let src = itemDiffFolder
    let fsReader = FakeFileSystemReader()
    fsReader.GetItem <- mockGetItem []
    fsReader.GetItems <- fun _ -> Ok newItems
    let fsWriter = FakeFileSystemWriter()
    let model = testModel |> withReg (Some (src.Path, src.Type, Move))

    let actual = seqResult (MainLogic.Action.put fsReader fsWriter false) model

    let expected = model.WithError (YankRegisterItemMissing (src.Path.Format model.PathFormat))
    assertAreEqual expected actual

[<Test>]
let ``Put handles error reading register item`` () =
    let src = itemDiffFolder
    let fsReader = FakeFileSystemReader()
    fsReader.GetItem <- (fun _ -> Error ex)
    fsReader.GetItems <- fun _ -> Ok newItems
    let fsWriter = FakeFileSystemWriter()
    let model = testModel |> withReg (Some (src.Path, src.Type, Move))

    let actual = seqResult (MainLogic.Action.put fsReader fsWriter false) model

    let expected = model.WithError (ActionError ("read yank register item", ex))
    assertAreEqual expected actual

[<TestCaseSource("putActionCases")>]
let ``Put item handles file system errors`` action =
    let src = itemDiffFolder
    let dest = itemSameFolderWith (if action = Shortcut then ".lnk" else "")
    let fsReader = FakeFileSystemReader()
    fsReader.GetItem <- mockGetItem [src]
    fsReader.GetItems <- fun _ -> Ok newItems
    let fsWriter = FakeFileSystemWriter()
    fsWriter.Move <- fun _ _ ->
        if action = Move then Error ex else failwith "Move should not be called"
    fsWriter.Copy <- fun _ _ ->
        if action = Copy then Error ex else failwith "Copy should not be called"
    fsWriter.CreateShortcut <- fun _ _ ->
        if action = Shortcut then Error ex else failwith "CreateShortcut should not be called"
    let item = Some (src.Path, src.Type, action)
    let model = testModel |> withReg item

    let actual = seqResult (MainLogic.Action.put fsReader fsWriter false) model

    let expectedAction = PutItem (action, src, dest.Path)
    let expected = model.WithError (ItemActionError (expectedAction, model.PathFormat, ex))
    assertAreEqual expected actual

// move tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Put item to move in different folder calls file sys move`` (overwrite: bool) =
    let src = itemDiffFolder
    let dest = itemSameFolder
    let fsReader = FakeFileSystemReader()
    fsReader.GetItem <- mockGetItem ([src] @ (if overwrite then [dest] else []))
    fsReader.GetItems <- fun _ -> Ok newItems
    let fsWriter = FakeFileSystemWriter()
    let mutable moved = []
    fsWriter.Move <- fun s d ->
        moved <- (s, d) :: moved
        Ok ()
    let model = testModel |> withReg (Some (src.Path, src.Type, Move))

    let actual = seqResult (MainLogic.Action.put fsReader fsWriter overwrite) model

    moved |> shouldEqual [src.Path, dest.Path]
    let expectedAction = PutItem (Move, src, dest.Path)
    let expected =
        { testModel with
            Directory = newItems
            Items = newItems
            Cursor = 1
            UndoStack = expectedAction :: testModel.UndoStack
            RedoStack = []
            Status = Some <| MainStatus.actionComplete expectedAction testModel.PathFormat
        }
    assertAreEqual expected actual

[<Test>]
let ``Put item to move in same folder returns error``() =
    let src = itemSameFolder
    let fsReader = FakeFileSystemReader()
    fsReader.GetItem <- mockGetItem [src]
    fsReader.GetItems <- fun _ -> Ok newItems
    let fsWriter = FakeFileSystemWriter()
    let model = testModel |> withReg (Some (src.Path, src.Type, Move))

    let actual = seqResult (MainLogic.Action.put fsReader fsWriter false) model

    let expected = model.WithError CannotMoveToSameFolder
    assertAreEqual expected actual

// undo move tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo move item moves it back`` curPathDifferent =
    let prevItem = newItems.[1]
    let curItem = oldItems.[1]
    let fsReader = FakeFileSystemReader()
    fsReader.GetItem <- fun _ -> Ok None
    fsReader.GetItems <- fun _ -> Ok newItems
    let fsWriter = FakeFileSystemWriter()
    let mutable moved = None
    fsWriter.Move <- fun s d ->
        moved <- Some (s, d)
        Ok ()
    let model =
        if curPathDifferent then
            testModel.WithLocation (createPath "/c/other")
        else
            testModel

    let actual = seqResult (MainLogic.Action.undoMove fsReader fsWriter prevItem curItem.Path) model

    moved |> shouldEqual (Some (curItem.Path, prevItem.Path))
    let expected =
        { testModel with
            Directory = newItems
            Items = newItems
            Cursor = 1
        } |> (fun m ->
            if curPathDifferent then
                { m with
                    BackStack = (createPath "/c/other", 0) :: m.BackStack
                    ForwardStack = []
                }
            else m
        )
    assertAreEqual expected actual

[<Test>]
let ``Undo move item when previous path is occupied returns error``() =
    let prevItem = newItems.[1]
    let curItem = oldItems.[1]
    let fsReader = FakeFileSystemReader()
    fsReader.GetItem <- fun p -> if p = prevItem.Path then Ok (Some prevItem) else Ok None
    let fsWriter = FakeFileSystemWriter()
    let model = testModel.WithLocation (createPath "/c/other")

    let actual = seqResult (MainLogic.Action.undoMove fsReader fsWriter prevItem curItem.Path) model

    let expected = model.WithError (CannotUndoMoveToExisting prevItem)
    assertAreEqual expected actual

[<Test>]
let ``Undo move item handles move error by returning error``() =
    let prevItem = newItems.[1]
    let curItem = oldItems.[1]
    let fsReader = FakeFileSystemReader()
    fsReader.GetItem <- fun _ -> Ok None
    let fsWriter = FakeFileSystemWriter()
    fsWriter.Move <- fun _ _ -> Error ex
    let model = testModel.WithLocation (createPath "/c/other")

    let actual = seqResult (MainLogic.Action.undoMove fsReader fsWriter prevItem curItem.Path) model

    let expectedAction = PutItem (Move, curItem, prevItem.Path)
    let expected = model.WithError (ItemActionError (expectedAction, model.PathFormat, ex))
    assertAreEqual expected actual

// copy tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Put item to copy in different folder calls file sys copy`` (overwrite: bool) =
    let src = itemDiffFolder
    let dest = itemSameFolder
    let fsReader = FakeFileSystemReader()
    fsReader.GetItem <- mockGetItem ([src] @ (if overwrite then [dest] else []))
    fsReader.GetItems <- fun _ -> Ok newItems
    let fsWriter = FakeFileSystemWriter()
    let mutable copied = []
    fsWriter.Copy <- fun s d ->
        copied <- (s, d) :: copied
        Ok ()
    let model = testModel |> withReg (Some (src.Path, src.Type, Copy))

    let actual = seqResult (MainLogic.Action.put fsReader fsWriter overwrite) model

    copied |> shouldEqual [(src.Path, dest.Path)]
    let expectedAction = PutItem (Copy, src, dest.Path)
    let expected =
        { testModel with
            Directory = newItems
            Items = newItems
            Cursor = 1
            UndoStack = expectedAction :: testModel.UndoStack
            RedoStack = []
            Status = Some <| MainStatus.actionComplete expectedAction model.PathFormat
        }
    assertAreEqual expected actual

[<TestCase(0)>]
[<TestCase(1)>]
[<TestCase(2)>]
let ``Put item to copy in same folder calls file sys copy with new name`` existingCopies =
    let src = itemSameFolder
    let existingPaths = List.init existingCopies (fun i -> (itemCopy i).Path)
    let newItems = List.append newItems [itemCopy existingCopies]
    let fsReader = FakeFileSystemReader()
    fsReader.GetItem <- mockGetItemFunc (fun p ->
        if p = src.Path || existingPaths |> List.contains p then Some src else None)
    fsReader.GetItems <- fun _ -> Ok newItems
    let fsWriter = FakeFileSystemWriter()
    let mutable copied = None
    fsWriter.Copy <- fun s d ->
        copied <- Some (s, d)
        Ok ()
    let model = testModel |> withReg (Some (src.Path, src.Type, Copy))

    let actual = seqResult (MainLogic.Action.put fsReader fsWriter false) model

    let destName = MainLogic.Action.getCopyName src.Name existingCopies
    let destPath = model.Location.Join destName
    copied |> shouldEqual (Some (src.Path, destPath))
    let expectedAction = PutItem (Copy, itemSameFolder, destPath)
    let expected =
        { testModel with
            Directory = newItems
            Items = newItems
            Cursor = newItems.Length - 1
            UndoStack = expectedAction :: testModel.UndoStack
            RedoStack = []
            Status = Some <| MainStatus.actionComplete expectedAction model.PathFormat
        }
    assertAreEqual expected actual

// undo copy tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo copy item when copy has same timestamp deletes copy`` curPathDifferent =
    let modified = Some (DateTime(2000, 1, 1))
    let original = { itemDiffFolder with Modified = modified }
    let copied = { itemSameFolder with Modified = modified }
    let fsReader = FakeFileSystemReader()
    fsReader.GetItem <- fun p -> if p = copied.Path then Ok (Some copied) else Ok None
    fsReader.GetItems <- fun _ -> Ok newItems
    let fsWriter = FakeFileSystemWriter()
    let mutable deleted = None
    fsWriter.Delete <- fun p ->
        deleted <- Some p
        Ok ()
    let model =
        if curPathDifferent then
            testModel.WithLocation (createPath "/c/other")
        else
            testModel

    let actual = seqResult (MainLogic.Action.undoCopy fsReader fsWriter original copied.Path) model

    deleted |> shouldEqual (Some copied.Path)
    let expected =
        if curPathDifferent then
            model
        else
            { testModel with
                Directory = newItems
                Items = newItems
                Cursor = 0
            }
    assertAreEqualWith expected actual (ignoreMembers ["Status"])

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo copy item when copy has different or no timestamp recycles copy`` hasTimestamp =
    let time = if hasTimestamp then Some (DateTime(2000, 1, 1)) else None
    let original = { itemDiffFolder with Modified = time }
    let copied = { itemSameFolder with Modified = time |> Option.map (fun t -> t.AddDays(1.0)) }
    let fsReader = FakeFileSystemReader()
    fsReader.GetItem <- fun p -> if p = copied.Path then Ok (Some copied) else Ok None
    fsReader.GetItems <- fun _ -> Ok newItems
    let fsWriter = FakeFileSystemWriter()
    let mutable recycled = None
    fsWriter.Recycle <- fun p ->
        recycled <- Some p
        Ok ()

    let actual = seqResult (MainLogic.Action.undoCopy fsReader fsWriter original copied.Path) testModel

    recycled |> shouldEqual (Some copied.Path)
    let expected =
        { testModel with
            Directory = newItems
            Items = newItems
            Cursor = 0
        }
    assertAreEqual expected actual

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo copy item handles errors by returning error and consuming action`` throwOnGetItem =
    let original = itemDiffFolder
    let copied = itemSameFolder
    let fsReader = FakeFileSystemReader()
    fsReader.GetItem <- fun _ -> if throwOnGetItem then Error ex else Ok None
    let fsWriter = FakeFileSystemWriter()
    fsWriter.Recycle <- fun _ -> Error ex
    fsWriter.Delete <- fun _ -> Error ex

    let actual = seqResult (MainLogic.Action.undoCopy fsReader fsWriter original copied.Path) testModel

    let action = DeletedItem (copied, false)
    let expected = testModel.WithError (ItemActionError (action, testModel.PathFormat, ex))
    assertAreEqual expected actual

// shortcut tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Put shortcut calls file sys create shortcut`` overwrite =
    let target = itemDiffFolder
    let shortcut = itemSameFolderWith ".lnk"
    let fsReader = FakeFileSystemReader()
    fsReader.GetItem <- mockGetItem ([target] @ (if overwrite then [shortcut] else []))
    let newItems = newItems @ [shortcut]
    fsReader.GetItems <- fun _ -> Ok newItems
    let fsWriter = FakeFileSystemWriter()
    let mutable created = []
    fsWriter.CreateShortcut <- fun target dest ->
        created <- (target, dest) :: created
        Ok ()
    let model = testModel |> withReg (Some (target.Path, target.Type, Shortcut))

    let actual = seqResult (MainLogic.Action.put fsReader fsWriter overwrite) model

    created |> shouldEqual [(target.Path, shortcut.Path)]
    let expectedAction = PutItem (Shortcut, target, shortcut.Path)
    let expected =
        { testModel with
            Directory = newItems
            Items = newItems
            Cursor = newItems.Length - 1
            UndoStack = expectedAction :: testModel.UndoStack
            RedoStack = []
            Status = Some <| MainStatus.actionComplete expectedAction model.PathFormat
        }
    assertAreEqual expected actual

// undo shortcut tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo create shortcut deletes shortcut`` curPathDifferent =
    let shortcut = itemSameFolder
    let fsReader = FakeFileSystemReader()
    fsReader.GetItems <- fun _ -> Ok newItems
    let fsWriter = FakeFileSystemWriter()
    let mutable deleted = None
    fsWriter.Delete <- fun p ->
        deleted <- Some p
        Ok ()
    let model =
        if curPathDifferent then
            testModel.WithLocation (createPath "/c/other")
        else
            testModel

    let actual = MainLogic.Action.undoShortcut fsReader fsWriter shortcut.Path model
                 |> assertOk

    deleted |> shouldEqual (Some shortcut.Path)
    let expected =
        if curPathDifferent then
            model
        else
            { model with
                Directory = newItems
                Items = newItems
                Cursor = 0
            }
    assertAreEqual expected actual

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo create shortcut handles errors by returning error and consuming action`` throwOnDelete =
    let shortcut = { itemSameFolder with Type = File }
    let fsReader = FakeFileSystemReader()
    fsReader.GetItems <- fun _ -> Error ex
    let fsWriter = FakeFileSystemWriter()
    fsWriter.Delete <- fun _ -> if throwOnDelete then Error ex else Ok ()

    let actual = MainLogic.Action.undoShortcut fsReader fsWriter shortcut.Path testModel

    let action = DeletedItem (shortcut, false)
    let expected = Error (ItemActionError (action, testModel.PathFormat, ex))
    assertAreEqual expected actual
