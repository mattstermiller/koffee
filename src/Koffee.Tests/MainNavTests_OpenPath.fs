module Koffee.MainNavTests_OpenPath

open NUnit.Framework
open Koffee.Main

[<TestCase(false)>]
[<TestCase(true)>]
let ``Opening a valid path updates model correctly`` setCursor =
    let fs = FakeFileSystem [
        folder "folder" [
            file "file 1"
            file "file 2"
            file "file 3"
        ]
        file "file 1"
    ]
    let path = createPath "/c/folder"
    let cursor = if setCursor then CursorToIndex 2 else CursorStay
    let items = ["/c/file 1"; "/c/file 2"] |> List.map createFile
    let model =
        { testModel with
            Directory = items
            Items = items
            SelectedItems = [items.[1]]
        }

    let actual = Nav.openPath fs path cursor model |> assertOk

    let expectedItems = fs.ItemsIn (string path)
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            SelectedItems = []
            Cursor = if setCursor then 2 else 0
            BackStack = (model.Location, model.Cursor) :: model.BackStack
            ForwardStack = []
        }
        |> MainModel.withLocation path
        |> withLocationOnHistory
    assertAreEqual expected actual

[<TestCase(false)>]
[<TestCase(true)>]
let ``Opening same path does not modify navigation history or selected items`` setCursor =
    let fs = FakeFileSystem [
        folder "folder" []
        file "file 1"
        file "file 2"
    ]
    let path = createPath "/c"
    let cursor = if setCursor then CursorToIndex 2 else CursorStay
    let items = ["/c/file 1"; "/c/file 2"] |> List.map createFile
    let model =
        { testModel with
            Directory = items
            Items = items
            SelectedItems = [items.[1]]
        }

    let actual = Nav.openPath fs path cursor model |> assertOk

    let expectedItems = fs.ItemsIn "/c"
    let expected =
        { model with
            Directory = expectedItems
            Items = expectedItems
            Cursor = if setCursor then 2 else 0
        }
        |> MainModel.withLocation path
        |> withLocationOnHistory
    assertAreEqual expected actual

[<Test>]
let ``Opening a path that throws on GetItems returns error``() =
    let fs = FakeFileSystem [
        folder "inaccessible" []
        file "file"
    ]
    let path = createPath "/c/inaccessible"
    fs.AddExnPath false ex path
    let model = testModel

    let res = Nav.openPath fs path CursorStay model

    let expected = Error (MainStatus.CouldNotOpenPath (path, ex))
    assertAreEqual expected res

[<Test>]
let ``Opening same path that throws on GetItems sets empty item and error status``() =
    let fs = FakeFileSystem [
        folder "folder" []
        file "file"
    ]
    let path = createPath "/c"
    fs.AddExnPath false ex path
    let items = fs.ItemsIn "/c"
    let model =
        { testModel with
            Directory = items
            Items = items
            SelectedItems = [items.[1]]
        }

    let actual = Nav.openPath fs path CursorStay model |> assertOk

    let expectedError = MainStatus.CouldNotOpenPath (path, ex)
    let expected =
        { model with
            Directory = []
            Items = Item.EmptyFolderWithMessage (expectedError.Message model.PathFormat) path
            SelectedItems = []
        }
        |> MainModel.withError expectedError
    assertAreEqual expected actual

[<Test>]
let ``Open Parent when in folder opens parent and moves cursor to folder that was open`` () =
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
let ``Open Parent when in drive opens root and moves cursor to drive that was open`` () =
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
