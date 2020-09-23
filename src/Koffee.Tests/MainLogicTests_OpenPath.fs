module Koffee.MainLogicTests_OpenPath

open NUnit.Framework
open Acadian.FSharp

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
        History = testModel.History.WithPath (createPath "/c")
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
    fs.AddExn ex "/c/inaccessible"

    let path =
        match case.GetPath with
        | Same -> "/c"
        | Different -> "/c/different"
        | Inaccessible -> "/c/inaccessible"
        |> createPath
    let res = MainLogic.Nav.openPath fs path case.Select model

    let expected =
        match case.GetPath with
        | Same ->
            let items = fs.ItemsIn "/c"
            Ok { model.WithLocation path with
                    Directory = items
                    Items = items
                    Cursor = case.ExpectedCursor |? model.Cursor
               }
        | Different ->
            let items = fs.ItemsIn "/c/different"
            Ok { model.WithLocation path with
                    Directory = items
                    Items = items
                    Cursor = case.ExpectedCursor |? model.Cursor
                    BackStack = (model.Location, model.Cursor) :: model.BackStack
                    ForwardStack = []
                    History = model.History.WithPath path
               }
        | Inaccessible ->
            Error (ActionError ("open path", ex))

    assertAreEqual expected res

[<Test>]
let ``Opening a valid path updates model correctly``() =
    test { GetPath = Different
           Select = (SelectIndex 1)
           ExpectedCursor = Some 1 }

[<Test>]
let ``Opening same path does not modify navigation history``() =
    test { GetPath = Same
           Select = (SelectIndex 1)
           ExpectedCursor = Some 1 }

[<Test>]
let ``Opening a path that throws on GetItems sets error status only``() =
    test { GetPath = Inaccessible
           Select = (SelectIndex 1)
           ExpectedCursor = None }
