module Koffee.MainLogicTests_OpenPath

open NUnit.Framework
open FsUnitTyped
open Testing
open Utility

type PathCase =
    | Same
    | Different
    | Inaccessible

type TestCase = {
    GetPath: PathCase
    Select: SelectType
    ExpectedCursor: int option
}

let oldNodes = [
    createNode "/c/old/old 1"
    createNode "/c/old/old 2"
    createNode "/c/old/old 3"
]

let newNodes = [
    createNode "/c/path/new 1"
    createNode "/c/path/new 2"
]

let createModel () =
    let model = createBaseTestModel()
    model.Nodes <- oldNodes
    model.Path <- createPath "/c/old"
    model.Cursor <- 2
    model

let ex = System.UnauthorizedAccessException()

let test case =
    let path = createPath "/c/path"
    let getNodes _ =
        match case.GetPath with
        | Same -> (fun _ -> Ok oldNodes)
        | Different -> (fun _ -> Ok newNodes)
        | Inaccessible -> (fun _ -> Error ex)

    let model = createModel()
    match case.GetPath with
        | Same -> model.Path <- path
        | _ -> ()

    let res = MainLogic.Navigation.openPath getNodes false path case.Select model

    let expected = createModel()
    let expectedRes =
        match case.GetPath with
        | Same ->
            expected.Path <- path
            case.ExpectedCursor |> Option.iter (expected.set_Cursor)
            Ok ()
        | Different ->
            let prevPath = expected.Path
            let prevCursor = expected.Cursor
            expected.Nodes <- newNodes
            expected.Path <- path
            expected.Cursor <- case.ExpectedCursor |> Option.defaultValue prevCursor
            expected.BackStack <- (prevPath, prevCursor) :: expected.BackStack
            expected.ForwardStack <- []
            Ok ()
        | Inaccessible ->
            Error (ActionError ("open path", ex))

    res |> shouldEqual expectedRes
    assertAreEqual expected model

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
