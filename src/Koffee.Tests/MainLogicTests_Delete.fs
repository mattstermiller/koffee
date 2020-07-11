module Koffee.MainLogicTests_Delete

open NUnit.Framework
open FsUnitTyped

let oldItems = [
    createFolder "/c/path/one"
    createFolder "/c/path/two"
    createFolder "/c/path/three"
]

let newItems = [
    oldItems.[0]
    oldItems.[2]
]

let testModel =
    { baseModel.WithLocation (createPath "/c/path") with
        Directory = oldItems
        Items = oldItems
        Cursor = 1
    }

let ex = System.UnauthorizedAccessException() :> exn


[<TestCase(true)>]
[<TestCase(false)>]
let ``Delete calls correct file sys func and sets message`` permanent =
    let fsWriter = FakeFileSystemWriter()
    let mutable deleted = None
    fsWriter.Delete <- fun p -> deleted <- Some p; Ok ()
    let mutable recycled = None
    fsWriter.Recycle <- fun p -> recycled <- Some p; Ok ()
    let item = oldItems.[1]

    let actual = seqResult (MainLogic.Action.delete fsWriter item permanent) testModel

    if permanent then
        deleted |> shouldEqual (Some item.Path)
        recycled |> shouldEqual None
    else
        deleted |> shouldEqual None
        recycled |> shouldEqual (Some item.Path)
    let expectedAction = DeletedItem (oldItems.[1], permanent)
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
let ``Delete handles error by returning error``() =
    let fsWriter = FakeFileSystemWriter()
    fsWriter.Delete <- fun _ -> Error ex
    let item = oldItems.[1]

    let actual = seqResult (MainLogic.Action.delete fsWriter item true) testModel

    let expectedAction = (DeletedItem (oldItems.[1], true))
    let expected = testModel.WithError (ItemActionError (expectedAction, testModel.PathFormat, ex))
    assertAreEqual expected actual
