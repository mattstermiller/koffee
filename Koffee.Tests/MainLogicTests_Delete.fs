module Koffee.MainLogicTests_Delete

open NUnit.Framework
open FsUnitTyped
open Testing
open KellermanSoftware.CompareNetObjects

let oldNodes = [
    createNode "/c/path/one"
    createNode "/c/path/two"
    createNode "/c/path/three"
]

let newNodes = [
    oldNodes.[0]
    oldNodes.[2]
]

let createModel () =
    let model = createBaseTestModel()
    model.Path <- createPath "/c/path"
    model.Nodes <- oldNodes
    model.Cursor <- 1
    model

let ex = System.UnauthorizedAccessException() :> exn


[<TestCase(true)>]
[<TestCase(false)>]
let ``Delete calls correct file sys func and sets message`` permanent =
    let fsReader = FakeFileSystemReader()
    fsReader.GetNodes <- fun _ _ -> Ok newNodes
    let fsWriter = FakeFileSystemWriter()
    let mutable deleted = None
    fsWriter.Delete <- fun p -> deleted <- Some p; Ok ()
    let mutable recycled = None
    fsWriter.Recycle <- fun p -> recycled <- Some p; Ok ()
    let model = createModel()
    let node = oldNodes.[1]
    let res = MainLogic.Action.delete_m fsReader fsWriter node permanent model |> Async.RunSynchronously

    res |> shouldEqual (Ok ())
    if permanent then
        deleted |> shouldEqual (Some node.Path)
        recycled |> shouldEqual None
    else
        deleted |> shouldEqual None
        recycled |> shouldEqual (Some node.Path)
    let expectedAction = DeletedItem (oldNodes.[1], permanent)
    let expected = createModel()
    expected.Nodes <- newNodes
    expected.Cursor <- 1
    expected.UndoStack <- expectedAction :: expected.UndoStack
    expected.RedoStack <- []
    expected.Status <- Some <| MainStatus.actionComplete expectedAction model.PathFormat
    assertAreEqual expected model

[<Test>]
let ``Delete handles error by returning error``() =
    let fsReader = FakeFileSystemReader()
    fsReader.GetNodes <- fun _ _ -> Ok newNodes
    let fsWriter = FakeFileSystemWriter()
    fsWriter.Delete <- fun _ -> Error ex
    let model = createModel()
    let node = oldNodes.[1]
    let res = MainLogic.Action.delete_m fsReader fsWriter node true model |> Async.RunSynchronously

    let expectedAction = (DeletedItem (oldNodes.[1], true))
    res |> shouldEqual (Error (ItemActionError (expectedAction, model.PathFormat, ex)))
    let expected = createModel()
    CompareLogic() |> ignoreMembers ["Status"] |> assertAreEqualWith expected model
