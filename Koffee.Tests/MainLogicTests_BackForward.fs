module Koffee.MainLogicTests_BackForward

open NUnit.Framework
open FsUnitTyped
open Testing

type TestResult = {
    Path: string
    Cursor: int
    BackStack: (string * int) list
    ForwardStack: (string * int) list
}

let history handler backStack forwardStack =
    let model = createBaseTestModel()
    model.BackStack <- backStack |> List.map (fun (p, c) -> (createPath ("/c/" + p), c))
    model.ForwardStack <- forwardStack |> List.map (fun (p, c) -> (createPath ("/c/" + p), c))
    model.Path <- createPath "/c/path"
    model.Cursor <- 1

    let openPath path select (model: MainModel) =
        model.BackStack <- (model.Path, model.Cursor) :: model.BackStack
        model.ForwardStack <- []
        model.Path <- path
        model.Cursor <-
            match select with
            | SelectIndex c -> c
            | _ -> failwith "unexpected select type"

    handler openPath model

    let pathStr (path: Path, c) = (path.Name, c)
    { Path = model.Path.Name
      Cursor = model.Cursor
      BackStack = model.BackStack |> List.map pathStr
      ForwardStack = model.ForwardStack |> List.map pathStr }

let back = history MainLogic.Navigation.back
let forward = history MainLogic.Navigation.forward

[<Test>]
let ``Back without history does nothing``() =
    back [] ["fwd", 2]
    |> shouldEqual { Path = "path"
                     Cursor = 1
                     BackStack = []
                     ForwardStack = ["fwd", 2] }

[<Test>]
let ``Back with simple history changes path and stacks``() =
    back ["back", 2] []
    |> shouldEqual { Path = "back"
                     Cursor = 2
                     BackStack = []
                     ForwardStack = ["path", 1] }

[<Test>]
let ``Back with more history changes path and stacks``() =
    back ["back1", 2; "back2", 3] ["fwd1", 4; "fwd2", 5]
    |> shouldEqual { Path = "back1"
                     Cursor = 2
                     BackStack = ["back2", 3]
                     ForwardStack = ["path", 1; "fwd1", 4; "fwd2", 5] }

[<Test>]
let ``Forward without history does nothing``() =
    forward ["back", 2] []
    |> shouldEqual { Path = "path"
                     Cursor = 1
                     BackStack = ["back", 2]
                     ForwardStack = [] }

[<Test>]
let ``Forward with simple history changes path and stacks``() =
    forward [] ["fwd", 2]
    |> shouldEqual { Path = "fwd"
                     Cursor = 2
                     BackStack = ["path", 1]
                     ForwardStack = [] }

[<Test>]
let ``Forward with more history changes path and stacks``() =
    forward ["back1", 2; "back2", 3] ["fwd1", 4; "fwd2", 5]
    |> shouldEqual { Path = "fwd1"
                     Cursor = 4
                     BackStack = ["path", 1; "back1", 2; "back2", 3]
                     ForwardStack = ["fwd2", 5] }
