module Koffee.MainLogicTests_BackForward

open NUnit.Framework
open FsUnitTyped

type TestResult = {
    Path: string
    Cursor: int
    BackStack: (string * int) list
    ForwardStack: (string * int) list
}

let history handler backStack forwardStack =
    let model =
        { testModel with
            BackStack = backStack |> List.map (fun (p, c) -> (createPath ("/c/" + p), c))
            ForwardStack = forwardStack |> List.map (fun (p, c) -> (createPath ("/c/" + p), c))
            Cursor = 1
        } |> withLocation "/c/path"
    let items = List.init 6 (sprintf "file%i" >> file)
    let fs = FakeFileSystem [
        folder "back" items
        folder "fwd" items
        folder "path" []
    ]

    let actual = handler fs model |> assertOk

    let pathStr (path: Path, c) = (path.Name, c)
    { Path = actual.Location.Name
      Cursor = actual.Cursor
      BackStack = actual.BackStack |> List.map pathStr
      ForwardStack = actual.ForwardStack |> List.map pathStr
    }

let back = history MainLogic.Nav.back
let forward = history MainLogic.Nav.forward

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
    back ["back", 2; "back2", 3] ["fwd", 4; "fwd2", 5]
    |> shouldEqual { Path = "back"
                     Cursor = 2
                     BackStack = ["back2", 3]
                     ForwardStack = ["path", 1; "fwd", 4; "fwd2", 5] }

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
    forward ["back", 2; "back2", 3] ["fwd", 4; "fwd2", 5]
    |> shouldEqual { Path = "fwd"
                     Cursor = 4
                     BackStack = ["path", 1; "back", 2; "back2", 3]
                     ForwardStack = ["fwd2", 5] }
