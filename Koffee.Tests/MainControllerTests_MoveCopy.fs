module Koffee.MainControllerTests_MoveCopy

open System
open System.Windows.Input
open FSharp.Desktop.UI
open NUnit.Framework
open FsUnitTyped
open Foq
open KellermanSoftware.CompareNetObjects
open Testing

let nodeSameFolder = createNode "path" "two"
let nodeDiffFolder = createNode "other" "two"

let oldNodes = [
    createNode "path" "one"
    createNode "path" "three"
]

let nodeCopy num =
    createNode "path" (MainController.GetCopyName "two" num)

let newNodes = [
    createNode "path" "one"
    createNode "path" "two"
    nodeCopy 0
    nodeCopy 1
    nodeCopy 2
    createNode "path" "three"
]

let createModel () =
    let model = createBaseTestModel()
    model.Path <- createPath "path"
    model.Nodes <- oldNodes
    model.Cursor <- 0
    model

let ex = UnauthorizedAccessException()

let fileSysMock () = baseFileSysMock newNodes

let createUnauthorizedFileSys () =
    fileSysMock()
        .Setup(fun x -> <@ x.Move (any()) (any()) @>).Raises(ex)
        .Setup(fun x -> <@ x.Copy (any()) (any()) @>).Raises(ex)
        .Create()

let createController fileSys =
    let settingsFactory () = Mock.Of<Mvc<SettingsEvents, SettingsModel>>()
    MainController(fileSys, settingsFactory)


[<TestCase(false)>]
[<TestCase(true)>]
let ``Put item to move in different folder calls file sys move`` (overwrite: bool) =
    let fileSys =
        fileSysMock()
            .Setup(fun x -> <@ x.Exists (any()) @>).Returns(overwrite)
            .Create()
    let contr = createController fileSys
    let model = createModel()
    model.ItemBuffer <- Some (nodeDiffFolder, Move)
    if overwrite then
        model.CommandInputMode <- Some (Confirm Overwrite)
        contr.CommandCharTyped 'y' model |> Async.RunSynchronously
    else
        contr.Put false model |> Async.RunSynchronously

    let oldPath = nodeDiffFolder.Path
    let newPath = nodeSameFolder.Path
    verify <@ fileSys.Move oldPath newPath @> once
    let expectedAction = MovedItem (nodeDiffFolder, newPath)
    let expected = createModel()
    expected.Nodes <- newNodes
    expected.Cursor <- 1
    expected.UndoStack <- expectedAction :: expected.UndoStack
    expected.RedoStack <- []
    expected.Status <- Some <| MainController.ActionCompleteStatus expectedAction model.PathFormat
    assertAreEqual expected model


[<Test>]
let ``Put item to move in different folder with item of same name prompts for overwrite``() =
    let fileSys =
        fileSysMock()
            .Setup(fun x -> <@ x.Exists (any()) @>).Returns(true)
            .Create()
    let contr = createController fileSys
    let item = Some (nodeDiffFolder, Move)
    let model = createModel()
    model.ItemBuffer <- item
    contr.Put false model |> Async.RunSynchronously

    verify <@ fileSys.Move (any()) (any()) @> never
    let expected = createModel()
    expected.ItemBuffer <- item
    expected.CommandInputMode <- Some (Confirm Overwrite)
    assertAreEqual expected model


[<Test>]
let ``Put item to move in same folder gives same-folder message``() =
    let fileSys = fileSysMock().Create()
    let contr = createController fileSys
    let item = Some (nodeSameFolder, Move)
    let model = createModel()
    model.ItemBuffer <- item
    contr.Put false model |> Async.RunSynchronously

    verify <@ fileSys.Move (any()) (any()) @> never
    let expected = createModel()
    expected.ItemBuffer <- item
    expected.Status <- Some <| MainController.CannotMoveToSameFolderStatus
    assertAreEqual expected model


[<Test>]
let ``Put item to move handles error by setting error status``() =
    let fileSys = createUnauthorizedFileSys()
    let contr = createController fileSys
    let item = Some (nodeDiffFolder, Move)
    let model = createModel()
    model.ItemBuffer <- item
    contr.Put false model |> Async.RunSynchronously

    let expectedAction = MovedItem (nodeDiffFolder, nodeSameFolder.Path)
    let expected = createModel()
    expected.ItemBuffer <- item
    expected |> MainController.SetActionExceptionStatus expectedAction ex
    assertAreEqual expected model



[<TestCase(false)>]
[<TestCase(true)>]
let ``Put item to copy in different folder calls file sys copy`` (overwrite: bool) =
    let fileSys =
        fileSysMock()
            .Setup(fun x -> <@ x.Exists (any()) @>).Returns(overwrite)
            .Create()
    let contr = createController fileSys
    let model = createModel()
    model.ItemBuffer <- Some (nodeDiffFolder, Copy)
    if overwrite then
        model.CommandInputMode <- Some (Confirm Overwrite)
        contr.CommandCharTyped 'y' model |> Async.RunSynchronously
    else
        contr.Put false model |> Async.RunSynchronously

    let curPath = nodeDiffFolder.Path
    let newPath = nodeSameFolder.Path
    verify <@ fileSys.Copy curPath newPath @> once
    let expectedAction = CopiedItem (nodeDiffFolder, newPath)
    let expected = createModel()
    expected.Nodes <- newNodes
    expected.Cursor <- 1
    expected.UndoStack <- expectedAction :: expected.UndoStack
    expected.RedoStack <- []
    expected.Status <- Some <| MainController.ActionCompleteStatus expectedAction model.PathFormat
    assertAreEqual expected model


[<Test>]
let ``Put item to copy in different folder with item of same name prompts for overwrite``() =
    let fileSys =
        fileSysMock()
            .Setup(fun x -> <@ x.Exists (any()) @>).Returns(true)
            .Create()
    let contr = createController fileSys
    let item = Some (nodeDiffFolder, Copy)
    let model = createModel()
    model.ItemBuffer <- item
    contr.Put false model |> Async.RunSynchronously

    verify <@ fileSys.Copy (any()) (any()) @> never
    let expected = createModel()
    expected.ItemBuffer <- item
    expected.CommandInputMode <- Some (Confirm Overwrite)
    assertAreEqual expected model


[<TestCase(0)>]
[<TestCase(1)>]
[<TestCase(2)>]
let ``Put item to copy in same folder calls file sys copy with new name`` existingCopies =
    let existingPaths = List.init existingCopies (fun i -> (nodeCopy i).Path)
    let fileSys =
        fileSysMock()
            .Setup(fun x -> <@ x.Exists (is(fun p -> List.contains p existingPaths)) @>).Returns(true)
            .Create()
    let contr = createController fileSys
    let item = Some (nodeSameFolder, Copy)
    let model = createModel()
    let path = model.Path
    model.ItemBuffer <- item
    contr.Put false model |> Async.RunSynchronously

    let oldPath = nodeSameFolder.Path
    let newPath = path.Join (MainController.GetCopyName nodeSameFolder.Name existingCopies)
    verify <@ fileSys.Copy oldPath newPath @> once
    let expectedAction = CopiedItem (nodeSameFolder, newPath)
    let expected = createModel()
    expected.Nodes <- newNodes
    expected.Cursor <- 2 + existingCopies
    expected.UndoStack <- expectedAction :: expected.UndoStack
    expected.RedoStack <- []
    expected.Status <- Some <| MainController.ActionCompleteStatus expectedAction model.PathFormat
    assertAreEqual expected model


[<Test>]
let ``Put item to copy handles error by setting error status``() =
    let fileSys = createUnauthorizedFileSys()
    let contr = createController fileSys
    let item = Some (nodeDiffFolder, Copy)
    let model = createModel()
    model.ItemBuffer <- item
    contr.Put false model |> Async.RunSynchronously

    let expectedAction = CopiedItem (nodeDiffFolder, nodeSameFolder.Path)
    let expected = createModel()
    expected.ItemBuffer <- item
    expected |> MainController.SetActionExceptionStatus expectedAction ex
    assertAreEqual expected model



[<TestCase(false)>]
[<TestCase(true)>]
let ``Confirm Overwrite answered 'n' with any item sets cancelled status`` isCopy =
    let fileSys =
        fileSysMock()
            .Setup(fun x -> <@ x.Exists (any()) @>).Returns(true)
            .Create()
    let contr = createController fileSys
    let action = if isCopy then Copy else Move
    let item = Some (nodeDiffFolder, action)
    let model = createModel()
    model.ItemBuffer <- item
    model.CommandInputMode <- Some (Confirm Overwrite)
    contr.CommandCharTyped 'n' model |> Async.RunSynchronously

    let expected = createModel()
    expected.ItemBuffer <- item
    expected.Status <- Some <| MainController.CancelledStatus
    assertAreEqual expected model


[<TestCase(false, 'h')>]
[<TestCase(false, 'z')>]
[<TestCase(true, 'h')>]
[<TestCase(true, 'z')>]
let ``Confirm Overwrite answered with any key besides 'y' or 'n' does nothing`` isCopy answer =
    let fileSys =
        fileSysMock()
            .Setup(fun x -> <@ x.Exists (any()) @>).Returns(true)
            .Create()
    let contr = createController fileSys
    let action = if isCopy then Copy else Move
    let item = Some (nodeDiffFolder, action)
    let model = createModel()
    model.ItemBuffer <- item
    model.CommandInputMode <- Some (Confirm Overwrite)
    model.CommandText <- "test"
    contr.CommandCharTyped answer model |> Async.RunSynchronously

    let expected = createModel()
    expected.ItemBuffer <- item
    expected.CommandInputMode <- Some (Confirm Overwrite)
    expected.CommandText <- ""
    assertAreEqual expected model

