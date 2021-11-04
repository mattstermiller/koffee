module Koffee.MainActionTests_Delete

open NUnit.Framework
open FsUnitTyped
open Koffee.Main

let createFs () =
    FakeFileSystem [
        file "file"
        file "other"
    ]

let testModel =
    let items = [
        createFile "/c/file"
        createFile "/c/other"
    ]
    { testModel with
        Directory = items
        Items = items
    }

[<TestCase(true)>]
[<TestCase(false)>]
let ``Delete calls correct file sys func and sets message`` permanent =
    let fs = createFs ()
    let item = fs.Item "/c/file"
    let model = testModel

    let actual = seqResult (Action.delete fs item permanent) model

    let expectedItems = [createFile "/c/other"]
    let expectedAction = DeletedItem (item, permanent)
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            UndoStack = expectedAction :: model.UndoStack
            RedoStack = []
            Status = Some <| MainStatus.actionComplete expectedAction model.PathFormat
        }
    assertAreEqual expected actual
    fs.ItemsShouldEqual [file "other"]
    fs.RecycleBin |> shouldEqual (if permanent then [] else [item])

[<Test>]
let ``Delete handles error by returning error``() =
    let fs = createFs ()
    let item = fs.Item "/c/file"
    fs.AddExnPath ex item.Path
    let model = testModel
    let expectedFs = fs.Items

    let actual = seqResult (Action.delete fs item true) model

    let expectedAction = (DeletedItem (item, true))
    let expected = model.WithError (ItemActionError (expectedAction, model.PathFormat, ex))
    assertAreEqual expected actual
    fs.Items |> shouldEqual expectedFs
