module Koffee.MainLogicTests_Delete

open NUnit.Framework
open FsUnitTyped
open Testing

let oldNodes = [
    createNode "/c/path/one"
    createNode "/c/path/two"
]

let createModel () =
    let model = createBaseTestModel()
    model.Path <- createPath "/c/path"
    model.Nodes <- oldNodes
    model.Cursor <- 1
    model

let ex = System.UnauthorizedAccessException()

[<TestCase(0)>]
[<TestCase(1)>]
let ``Recycle calls delete non-permanent and sets message`` cursor =
    let mutable deleted = None
    let delete node permanent _ = async { deleted <- Some (node, permanent) }
    let isRecyclable _ = true
    let model = createModel()
    model.Cursor <- cursor
    MainLogic.Action.recycle isRecyclable delete model |> Async.RunSynchronously

    deleted |> shouldEqual (Some (oldNodes.[cursor], false))
    let expected = createModel()
    expected.Cursor <- cursor
    expected.Status <- Some <| MainStatus.checkingIsRecyclable
    assertAreEqual expected model

[<Test>]
let ``Recycle sets status message when not recyclable``() =
    let delete _ _ _ = async { failwith "delete should not be called" }
    let isRecyclable _ = false
    let model = createModel()
    MainLogic.Action.recycle isRecyclable delete model |> Async.RunSynchronously

    let expected = createModel()
    expected.Status <- Some <| MainStatus.cannotRecycle oldNodes.[1]
    assertAreEqual expected model

[<Test>]
let ``Recycle handles error by setting error status``() =
    let delete _ _ _ = async { failwith "delete should not be called" }
    let isRecyclable _ = raise ex
    let model = createModel()
    MainLogic.Action.recycle isRecyclable delete model |> Async.RunSynchronously

    let expected = createModel()
    expected |> MainStatus.setActionExceptionStatus (DeletedItem (oldNodes.[1], false)) ex
    assertAreEqual expected model


[<TestCase(true)>]
[<TestCase(false)>]
let ``Delete calls correct file sys func and sets message`` permanent =
    let mutable deleted = None
    let fsDelete p = deleted <- Some p
    let mutable recycled = None
    let fsRecycle p = recycled <- Some p
    let mutable refreshed = false
    let refresh _ = refreshed <- true
    let model = createModel()
    let node = oldNodes.[0]
    MainLogic.Action.delete fsDelete fsRecycle refresh node permanent model |> Async.RunSynchronously

    if permanent then
        deleted |> shouldEqual (Some node.Path)
        recycled |> shouldEqual None
    else
        deleted |> shouldEqual None
        recycled |> shouldEqual (Some node.Path)
    refreshed |> shouldEqual true
    let expectedAction = DeletedItem (oldNodes.[0], permanent)
    let expected = createModel()
    expected.UndoStack <- expectedAction :: expected.UndoStack
    expected.RedoStack <- []
    expected.Status <- Some <| MainStatus.actionComplete expectedAction model.PathFormat
    assertAreEqual expected model

[<Test>]
let ``Delete handles error by setting error status``() =
    let fsDelete _ = raise ex
    let fsRecycle _ = failwith "recycle should not be called"
    let mutable refreshed = false
    let refresh _ = refreshed <- true
    let model = createModel()
    let node = oldNodes.[0]
    MainLogic.Action.delete fsDelete fsRecycle refresh node true model |> Async.RunSynchronously

    refreshed |> shouldEqual false
    let expected = createModel()
    expected |> MainStatus.setActionExceptionStatus (DeletedItem (oldNodes.[0], true)) ex
    assertAreEqual expected model
