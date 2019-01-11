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
            createNode "/c/old/old 1"
            createNode "/c/old/old 2"
            createNode "/c/old/old 3"
        ]
        Location = createPath "/c/old"
        Cursor = 2
    }

let newNodes = [
    createNode "/c/path/new 1"
    createNode "/c/path/new 2"
]

let ex = System.UnauthorizedAccessException() :> exn

let test case =
    let fsReader = FakeFileSystemReader()
    fsReader.GetNodes <- fun _ _ ->
        match case.GetPath with
        | Same -> Ok model.Nodes
        | Different -> Ok newNodes
        | Inaccessible -> Error ex

    let path = createPath "/c/path"
    let loc =
        match case.GetPath with
        | Same -> path
        | _ -> model.Location
    let testModel = { model with Location = loc }

    let res = MainLogic.Navigation.openPath fsReader path case.Select testModel


    let expected =
        match case.GetPath with
        | Same ->
            Ok { model with
                    Location = path
                    Cursor = case.ExpectedCursor |? model.Cursor
               }
        | Different ->
            Ok { model with
                    Nodes = newNodes
                    Location = path
                    Cursor = case.ExpectedCursor |? model.Cursor
                    BackStack = (model.Location, model.Cursor) :: model.BackStack
                    ForwardStack = []
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
