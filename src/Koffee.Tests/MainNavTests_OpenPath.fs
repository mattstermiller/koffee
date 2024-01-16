module Koffee.MainNavTests_OpenPath

open NUnit.Framework
open Acadian.FSharp
open Koffee.Main

type PathCase =
    | Same
    | Different
    | Inaccessible

type TestCase = {
    GetPath: PathCase
    Select: SelectType
    ExpectedCursor: int option
}

let model =
    { testModel with
        Items = [
            createFolder "/c/file 1"
            createFolder "/c/file 2"
            createFolder "/c/file 3"
        ]
        Location = createPath "/c"
        Cursor = 2
        History = testModel.History |> History.withFolderPath 9 (createPath "/c")
    }

let test case =
    let fs = FakeFileSystem [
        folder "different" [
            file "diff1"
            file "diff2"
        ]
        folder "inaccessible" []
        file "file"
        file "newfile"
    ]
    fs.AddExn false ex "/c/inaccessible"

    let path =
        match case.GetPath with
        | Same -> "/c"
        | Different -> "/c/different"
        | Inaccessible -> "/c/inaccessible"
        |> createPath
    let res = Nav.openPath fs path case.Select model

    let expected =
        match case.GetPath with
        | Same ->
            let items = fs.ItemsIn "/c"
            Ok (
                { model with
                    Directory = items |> sortByPath
                    Items = items
                    Cursor = case.ExpectedCursor |? model.Cursor
                }
                |> MainModel.withLocation path
            )
        | Different ->
            let items = fs.ItemsIn "/c/different"
            Ok (
                { model with
                    Directory = items |> sortByPath
                    Items = items
                    Cursor = case.ExpectedCursor |? model.Cursor
                    BackStack = (model.Location, model.Cursor) :: model.BackStack
                    ForwardStack = []
                    History = model.History |> History.withFolderPath 9 path
                }
                |> MainModel.withLocation path
            )
        | Inaccessible ->
            Error (MainStatus.CouldNotOpenPath (path, model.PathFormat, ex))

    assertAreEqual expected res

[<TestCase(false)>]
[<TestCase(true)>]
let ``Opening a valid path updates model correctly`` setCursor =
    test { GetPath = Different
           Select = if setCursor then SelectIndex 1 else SelectNone
           ExpectedCursor = Some (if setCursor then 1 else 0) }

[<TestCase(false)>]
[<TestCase(true)>]
let ``Opening same path does not modify navigation history`` setCursor =
    test { GetPath = Same
           Select = if setCursor then SelectIndex 1 else SelectNone
           ExpectedCursor = if setCursor then Some 1 else None }

[<Test>]
let ``Opening a path that throws on GetItems sets error status only``() =
    test { GetPath = Inaccessible
           Select = SelectIndex 1
           ExpectedCursor = None }

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
