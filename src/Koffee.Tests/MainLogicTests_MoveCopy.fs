module Koffee.MainLogicTests_MoveCopy

open System
open NUnit.Framework
open FsUnitTyped

let nodeSameFolder = createNode "/c/path/file 2"
let nodeSameFolderWith ext = createNode ("/c/path/file 2" + ext)
let nodeDiffFolder = createNode "/c/other/file 2"

let oldNodes = [
    createNode "/c/path/file 1"
    createNode "/c/path/file 3"
]

let newNodes = [
    createNode "/c/path/file 1"
    nodeSameFolder
]

let nodeCopy num =
    createNode ("/c/path/" + (MainLogic.Action.getCopyName "file 2" num))

let modelPathNode = createNode "/c/path"

let testModel =
    { baseModel.WithLocation modelPathNode.Path with
        Nodes = oldNodes
        Cursor = 0
    }

let mockGetNodeFunc nodeFunc path =
    if path = modelPathNode.Path then Ok (Some modelPathNode)
    else Ok (nodeFunc path)

let mockGetNode nodeToReturn = mockGetNodeFunc (fun _ -> nodeToReturn)

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
    let src = nodeDiffFolder
    let dest = { nodeSameFolder with IsHidden = existingHidden }
    let mutable loadedHidden = None
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- mockGetNode (Some dest)
    fsReader.GetNodes <- fun sh path ->
        loadedHidden <- Some sh
        Ok newNodes
    let fsWriter = FakeFileSystemWriter()
    let item = Some (src, action)
    let model = { testModel with YankRegister = item }

    let actual = seqResult (MainLogic.Action.put fsReader fsWriter false) model

    let expected =
        { testModel with
            YankRegister = item
            Nodes = newNodes
            Cursor = 1
            InputMode = Some (Confirm (Overwrite (action, src, dest)))
        }
    assertAreEqual expected actual
    loadedHidden |> shouldEqual (Some existingHidden)

[<TestCaseSource("putActionCases")>]
let ``Put item handles errors`` action =
    let src = nodeDiffFolder
    let dest = nodeSameFolderWith (if action = Shortcut then ".lnk" else "")
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- mockGetNode None
    fsReader.GetNodes <- fun _ _ -> Ok newNodes
    let fsWriter = FakeFileSystemWriter()
    fsWriter.Move <- fun _ _ ->
        if action = Move then Error ex else failwith "Move should not be called"
    fsWriter.Copy <- fun _ _ ->
        if action = Copy then Error ex else failwith "Copy should not be called"
    fsWriter.CreateShortcut <- fun _ _ ->
        if action = Shortcut then Error ex else failwith "CreateShortcut should not be called"
    let item = Some (src, action)
    let model = { testModel with YankRegister = item }

    let actual = seqResult (MainLogic.Action.put fsReader fsWriter false) model

    let expectedAction = PutItem (action, src, dest.Path)
    let expected = model.WithError (ItemActionError (expectedAction, model.PathFormat, ex))
    assertAreEqual expected actual

// move tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Put item to move in different folder calls file sys move`` (overwrite: bool) =
    let src = nodeDiffFolder
    let dest = nodeSameFolder
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- mockGetNode (if overwrite then Some dest else None)
    fsReader.GetNodes <- fun _ _ -> Ok newNodes
    let fsWriter = FakeFileSystemWriter()
    let mutable moved = []
    fsWriter.Move <- fun s d ->
        moved <- (s, d) :: moved
        Ok ()
    let model = { testModel with YankRegister = Some (src, Move) }

    let actual = seqResult (MainLogic.Action.put fsReader fsWriter overwrite) model

    moved |> shouldEqual [src.Path, dest.Path]
    let expectedAction = PutItem (Move, src, dest.Path)
    let expected =
        { testModel with
            Nodes = newNodes
            Cursor = 1
            UndoStack = expectedAction :: testModel.UndoStack
            RedoStack = []
            Status = Some <| MainStatus.actionComplete expectedAction testModel.PathFormat
        }
    assertAreEqual expected actual

[<Test>]
let ``Put item to move in same folder returns error``() =
    let src = nodeSameFolder
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- mockGetNode None
    fsReader.GetNodes <- fun _ _ -> Ok newNodes
    let fsWriter = FakeFileSystemWriter()
    let item = Some (src, Move)
    let model = { testModel with YankRegister = item }

    let actual = seqResult (MainLogic.Action.put fsReader fsWriter false) model

    let expected = model.WithError CannotMoveToSameFolder
    assertAreEqual expected actual

// undo move tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo move item moves it back`` curPathDifferent =
    let prevNode = newNodes.[1]
    let curNode = oldNodes.[1]
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- fun _ -> Ok None
    fsReader.GetNodes <- fun _ _ -> Ok newNodes
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

    let actual = seqResult (MainLogic.Action.undoMove fsReader fsWriter prevNode curNode.Path) model

    moved |> shouldEqual (Some (curNode.Path, prevNode.Path))
    let expected =
        { testModel with
            Nodes = newNodes
            Cursor = 1
        }
    let expected =
        if curPathDifferent then
            { expected with
                BackStack = (createPath "/c/other", 0) :: expected.BackStack
                ForwardStack = []
            }
        else
            expected
    assertAreEqual expected actual

[<Test>]
let ``Undo move item when previous path is occupied returns error``() =
    let prevNode = newNodes.[1]
    let curNode = oldNodes.[1]
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- fun p -> if p = prevNode.Path then Ok (Some prevNode) else Ok None
    let fsWriter = FakeFileSystemWriter()
    let model = testModel.WithLocation (createPath "/c/other")

    let actual = seqResult (MainLogic.Action.undoMove fsReader fsWriter prevNode curNode.Path) model

    let expected = model.WithError (CannotUndoMoveToExisting prevNode)
    assertAreEqual expected actual

[<Test>]
let ``Undo move item handles move error by returning error``() =
    let prevNode = newNodes.[1]
    let curNode = oldNodes.[1]
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- fun _ -> Ok None
    let fsWriter = FakeFileSystemWriter()
    fsWriter.Move <- fun _ _ -> Error ex
    let model = testModel.WithLocation (createPath "/c/other")

    let actual = seqResult (MainLogic.Action.undoMove fsReader fsWriter prevNode curNode.Path) model

    let expectedAction = PutItem (Move, curNode, prevNode.Path)
    let expected = model.WithError (ItemActionError (expectedAction, model.PathFormat, ex))
    assertAreEqual expected actual

// copy tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Put item to copy in different folder calls file sys copy`` (overwrite: bool) =
    let src = nodeDiffFolder
    let dest = nodeSameFolder
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- mockGetNode (if overwrite then Some dest else None)
    fsReader.GetNodes <- fun _ _ -> Ok newNodes
    let fsWriter = FakeFileSystemWriter()
    let mutable copied = []
    fsWriter.Copy <- fun s d ->
        copied <- (s, d) :: copied
        Ok ()
    let model = { testModel with YankRegister = Some (src, Copy) }

    let actual = seqResult (MainLogic.Action.put fsReader fsWriter overwrite) model

    copied |> shouldEqual [(src.Path, dest.Path)]
    let expectedAction = PutItem (Copy, src, dest.Path)
    let expected =
        { testModel with
            Nodes = newNodes
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
    let src = nodeSameFolder
    let existingPaths = List.init existingCopies (fun i -> (nodeCopy i).Path)
    let newNodes = List.append newNodes [nodeCopy existingCopies]
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- mockGetNodeFunc (fun p -> if existingPaths |> List.contains p then Some src else None)
    fsReader.GetNodes <- fun _ _ -> Ok newNodes
    let fsWriter = FakeFileSystemWriter()
    let mutable copied = None
    fsWriter.Copy <- fun s d ->
        copied <- Some (s, d)
        Ok ()
    let model = { testModel with YankRegister = Some (src, Copy) }

    let actual = seqResult (MainLogic.Action.put fsReader fsWriter false) model

    let destName = MainLogic.Action.getCopyName src.Name existingCopies
    let destPath = model.Location.Join destName
    copied |> shouldEqual (Some (src.Path, destPath))
    let expectedAction = PutItem (Copy, nodeSameFolder, destPath)
    let expected =
        { testModel with
            Nodes = newNodes
            Cursor = newNodes.Length - 1
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
    let original = { nodeDiffFolder with Modified = modified }
    let copied = { nodeSameFolder with Modified = modified }
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- fun p -> if p = copied.Path then Ok (Some copied) else Ok None
    fsReader.GetNodes <- fun _ _ -> Ok newNodes
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
                Nodes = newNodes
                Cursor = 0
            }
    assertAreEqualWith expected actual (ignoreMembers ["Status"])

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo copy item when copy has different or no timestamp recycles copy`` hasTimestamp =
    let time = if hasTimestamp then Some (DateTime(2000, 1, 1)) else None
    let original = { nodeDiffFolder with Modified = time }
    let copied = { nodeSameFolder with Modified = time |> Option.map (fun t -> t.AddDays(1.0)) }
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- fun p -> if p = copied.Path then Ok (Some copied) else Ok None
    fsReader.GetNodes <- fun _ _ -> Ok newNodes
    let fsWriter = FakeFileSystemWriter()
    let mutable recycled = None
    fsWriter.Recycle <- fun p ->
        recycled <- Some p
        Ok ()

    let actual = seqResult (MainLogic.Action.undoCopy fsReader fsWriter original copied.Path) testModel

    recycled |> shouldEqual (Some copied.Path)
    let expected =
        { testModel with
            Nodes = newNodes
            Cursor = 0
        }
    assertAreEqual expected actual

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo copy item handles errors by returning error and consuming action`` throwOnGetNode =
    let original = nodeDiffFolder
    let copied = nodeSameFolder
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- fun _ -> if throwOnGetNode then Error ex else Ok None
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
    let target = nodeDiffFolder
    let shortcut = nodeSameFolderWith ".lnk"
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- mockGetNode (if overwrite then Some shortcut else None)
    let newNodes = newNodes @ [shortcut]
    fsReader.GetNodes <- fun _ _ -> Ok newNodes
    let fsWriter = FakeFileSystemWriter()
    let mutable created = []
    fsWriter.CreateShortcut <- fun target dest ->
        created <- (target, dest) :: created
        Ok ()
    let model = { testModel with YankRegister = Some (target, Shortcut) }

    let actual = seqResult (MainLogic.Action.put fsReader fsWriter overwrite) model

    created |> shouldEqual [(target.Path, shortcut.Path)]
    let expectedAction = PutItem (Shortcut, target, shortcut.Path)
    let expected =
        { testModel with
            Nodes = newNodes
            Cursor = newNodes.Length - 1
            UndoStack = expectedAction :: testModel.UndoStack
            RedoStack = []
            Status = Some <| MainStatus.actionComplete expectedAction model.PathFormat
        }
    assertAreEqual expected actual

// undo shortcut tests

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo create shortcut deletes shortcut`` curPathDifferent =
    let shortcut = nodeSameFolder
    let fsReader = FakeFileSystemReader()
    fsReader.GetNodes <- fun _ _ -> Ok newNodes
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
                Nodes = newNodes
                Cursor = 0
            }
    assertAreEqual expected actual

[<TestCase(false)>]
[<TestCase(true)>]
let ``Undo create shortcut handles errors by returning error and consuming action`` throwOnDelete =
    let shortcut = { nodeSameFolder with Type = File }
    let fsReader = FakeFileSystemReader()
    fsReader.GetNodes <- fun _ _ -> Error ex
    let fsWriter = FakeFileSystemWriter()
    fsWriter.Delete <- fun _ -> if throwOnDelete then Error ex else Ok ()

    let actual = MainLogic.Action.undoShortcut fsReader fsWriter shortcut.Path testModel

    let action = DeletedItem (shortcut, false)
    let expected = Error (ItemActionError (action, testModel.PathFormat, ex))
    assertAreEqual expected actual
