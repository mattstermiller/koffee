module Koffee.MainLogicTests_Delete

open NUnit.Framework
open FsUnitTyped
open Testing
open KellermanSoftware.CompareNetObjects

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


[<TestCase(true)>]
[<TestCase(false)>]
let ``Delete calls correct file sys func and sets message`` permanent =
    let mutable deleted = None
    let fsDelete p = deleted <- Some p; Ok ()
    let mutable recycled = None
    let fsRecycle p = recycled <- Some p; Ok ()
    let mutable refreshed = false
    let refresh _ = refreshed <- true
    let model = createModel()
    let node = oldNodes.[0]
    let res = MainLogic.Action.delete fsDelete fsRecycle refresh node permanent model |> Async.RunSynchronously

    res |> shouldEqual (Ok ())
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
let ``Delete handles error by returning error``() =
    let fsDelete _ = Error ex
    let fsRecycle _ = failwith "recycle should not be called"
    let mutable refreshed = false
    let refresh _ = refreshed <- true
    let model = createModel()
    let node = oldNodes.[0]
    let res = MainLogic.Action.delete fsDelete fsRecycle refresh node true model |> Async.RunSynchronously

    let expectedAction = (DeletedItem (oldNodes.[0], true))
    res |> shouldEqual (Error (ItemActionError (expectedAction, model.PathFormat, ex)))
    refreshed |> shouldEqual false
    let expected = createModel()
    CompareLogic() |> ignoreMembers ["Status"] |> assertAreEqualWith expected model
