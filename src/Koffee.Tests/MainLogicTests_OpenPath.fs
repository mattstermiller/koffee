module Koffee.MainLogicTests_OpenPath

open NUnit.Framework
open FsUnitTyped
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
    { baseModel with
        Nodes = [
            createNode "/c/path/file 1"
            createNode "/c/path/file 2"
            createNode "/c/path/file 3"
        ]
        Location = createPath "/c/path"
        Cursor = 2
        History = baseModel.History.WithPath (createPath "/c/path")
    }

let newNodes = [
    createNode "/c/newpath/new 1"
    createNode "/c/newpath/new 2"
]

let ex = System.UnauthorizedAccessException() :> exn

let test case =
    let fsReader = FakeFileSystemReader()
    fsReader.GetNodes <- fun _ ->
        match case.GetPath with
        | Same -> Ok model.Nodes
        | Different -> Ok newNodes
        | Inaccessible -> Error ex

    let path =
        match case.GetPath with
        | Same -> model.Location
        | _ -> createPath "/c/newpath"
    let res = MainLogic.Nav.openPath fsReader path case.Select model

    let expected =
        match case.GetPath with
        | Same ->
            Ok { model.WithLocation path with
                    Cursor = case.ExpectedCursor |? model.Cursor
               }
        | Different ->
            Ok { model.WithLocation path with
                    Nodes = newNodes
                    Cursor = case.ExpectedCursor |? model.Cursor
                    BackStack = (model.Location, model.Cursor) :: model.BackStack
                    ForwardStack = []
                    History = model.History.WithPath path
               }
        | Inaccessible ->
            Error (ActionError ("open path", ex))

    res |> shouldEqual expected

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
let ``Opening a path that throws on GetNodes sets error status only``() =
    test { GetPath = Inaccessible
           Select = (SelectIndex 1)
           ExpectedCursor = None }
