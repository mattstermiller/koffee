module Koffee.MainNavTests_OpenPath

open NUnit.Framework
open Koffee.Main

[<TestCase(false)>]
[<TestCase(true)>]
let ``Opening a valid path updates model correctly`` setCursor =
    let fs = FakeFileSystem [
        folder "different" [
            file "diff1"
            file "diff2"
            file "diff3"
        ]
        file "file"
    ]
    let path = createPath "/c/different"
    let select = if setCursor then SelectIndex 2 else SelectNone
    let model = testModel

    let actual = Nav.openPath fs path select model |> assertOk

    let expectedItems = fs.ItemsIn (string path)
    let expected =
        { model with
            Directory = expectedItems |> sortByPath
            Items = expectedItems
            Cursor = if setCursor then 2 else 0
            BackStack = (model.Location, model.Cursor) :: model.BackStack
            ForwardStack = []
        }
        |> MainModel.withLocation path
        |> withLocationOnHistory
    assertAreEqual expected actual

[<TestCase(false)>]
[<TestCase(true)>]
let ``Opening same path does not modify navigation history`` setCursor =
    let fs = FakeFileSystem [
        folder "different" []
        file "file1"
        file "file2"
    ]
    let path = createPath "/c"
    let select = if setCursor then SelectIndex 2 else SelectNone
    let model = testModel

    let actual = Nav.openPath fs path select model |> assertOk

    let expectedItems = fs.ItemsIn "/c"
    let expected =
        { model with
            Directory = expectedItems |> sortByPath
            Items = expectedItems
            Cursor = if setCursor then 2 else 0
        }
        |> MainModel.withLocation path
        |> withLocationOnHistory
    assertAreEqual expected actual


[<Test>]
let ``Opening a path that throws on GetItems sets error status only``() =
    let fs = FakeFileSystem [
        folder "inaccessible" []
        file "file"
    ]
    let path = createPath "/c/inaccessible"
    fs.AddExnPath false ex path
    let model = testModel

    let res = Nav.openPath fs path SelectNone model

    let expected = Error (MainStatus.CouldNotOpenPath (path, model.PathFormat, ex))
    assertAreEqual expected res

[<Test>]
let ``Open Parent when in folder opens parent and selects folder that was open`` () =
    let fs = FakeFileSystem [
        folder "another" []
        folder "folder" []
    ]
    let model =
        { testModel with
            Directory = []
            Items = [Item.Empty]
        }
        |> withLocation "/c/folder"
    let expectedItems = fs.ItemsIn "/c"

    let actual = Nav.openParent fs model
                 |> assertOk

    let expected =
        { testModel with
            Directory = expectedItems
            Items = expectedItems
            Cursor = 1
        }
        |> withLocation "/c"
        |> withBack (model.Location, 0)
        |> withLocationOnHistory
    assertAreEqual expected actual

[<Test>]
let ``Open Parent when in drive opens root and selects drive that was open`` () =
    let fs = FakeFileSystem [
        drive 'c' []
        drive 'd' []
    ]
    let model =
        { testModel with
            Directory = []
            Items = [Item.Empty]
        }
        |> withLocation "/d"
    let expectedItems = fs.ItemsIn Path.Root

    let actual = Nav.openParent fs model
                 |> assertOk

    let expected =
        { testModel with
            Directory = expectedItems
            Items = expectedItems
            Cursor = 1
        }
        |> withLocation "/"
        |> withBack (model.Location, 0)
        |> withLocationOnHistory
    assertAreEqual expected actual
