module Koffee.MainLogicTests_Delete

open NUnit.Framework
open FsUnitTyped

let oldNodes = [
    createNode "/c/path/one"
    createNode "/c/path/two"
    createNode "/c/path/three"
]

let newNodes = [
    oldNodes.[0]
    oldNodes.[2]
]

let testModel =
    { baseModel.WithLocation (createPath "/c/path") with
        Nodes = oldNodes
        Cursor = 1
    }

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
    let node = oldNodes.[1]

    let actual = seqResult (MainLogic.Action.delete fsReader fsWriter node permanent) testModel

    if permanent then
        deleted |> shouldEqual (Some node.Path)
        recycled |> shouldEqual None
    else
        deleted |> shouldEqual None
        recycled |> shouldEqual (Some node.Path)
    let expectedAction = DeletedItem (oldNodes.[1], permanent)
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
let ``Delete handles error by returning error``() =
    let fsReader = FakeFileSystemReader()
    fsReader.GetNodes <- fun _ _ -> Ok newNodes
    let fsWriter = FakeFileSystemWriter()
    fsWriter.Delete <- fun _ -> Error ex
    let node = oldNodes.[1]

    let actual = seqResult (MainLogic.Action.delete fsReader fsWriter node true) testModel

    let expectedAction = (DeletedItem (oldNodes.[1], true))
    let expected = testModel.WithError (ItemActionError (expectedAction, testModel.PathFormat, ex))
    assertAreEqualWith expected actual (ignoreMembers ["Status"])
