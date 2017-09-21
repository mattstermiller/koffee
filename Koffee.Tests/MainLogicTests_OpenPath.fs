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
    createNode "old" "old 1"
    createNode "old" "old 2"
    createNode "old" "old 3"
]

let newNodes = [
    createNode "path" "new 1"
    createNode "path" "new 2"
]

let createModel () =
    let model = createBaseTestModel()
    model.Nodes <- oldNodes
    model.Path <- createPath "old"
    model.Cursor <- 2
    model

let ex = System.UnauthorizedAccessException()

let test case =
    let path = createPath "path"
    let getNodes _ =
        match case.GetPath with
        | Same -> (fun _ -> oldNodes)
        | Different -> (fun _ -> newNodes)
        | Inaccessible -> (fun _ -> raise ex)

    let model = createModel()
    match case.GetPath with
        | Same -> model.Path <- path
        | _ -> ()

    MainLogic.Navigation.openPath getNodes false path case.Select model

    let expected = createModel()
    match case.GetPath with
        | Same ->
            expected.Path <- path
            case.ExpectedCursor |> Option.iter (fun c ->
                expected.Cursor <- c)
        | Different ->
            let prevPath = expected.Path
            let prevCursor = expected.Cursor
            expected.Nodes <- newNodes
            expected.Path <- path
            expected.Cursor <- case.ExpectedCursor |> Option.coalesce prevCursor
            expected.BackStack <- (prevPath, prevCursor) :: expected.BackStack
            expected.ForwardStack <- []
        | Inaccessible ->
            expected.Status <- Some <| StatusType.fromExn "open path" ex

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
