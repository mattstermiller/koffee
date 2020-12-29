module Koffee.MainLogicTests_BackForward

open NUnit.Framework
open FsUnitTyped

type TestResult = {
    Path: string
    Cursor: int
    BackStack: (string * int) list
    ForwardStack: (string * int) list
}

let history handler count backStack forwardStack =
    let model =
        { testModel with
            BackStack = backStack |> List.map (fun (p, c) -> (createPath ("/c/" + p), c))
            ForwardStack = forwardStack |> List.map (fun (p, c) -> (createPath ("/c/" + p), c))
            Cursor = 1
            RepeatCommand = count
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

let back = history MainLogic.Nav.back None
let forward = history MainLogic.Nav.forward None
let backCount count = history MainLogic.Nav.back (Some count)
let forwardCount count = history MainLogic.Nav.forward (Some count)

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

[<Test>]
let ``Back 3 will move 3 times``() =
    backCount 3 ["back1", 2; "back2", 3; "back", 4; "back4", 5] ["fwd", 6]
    |> shouldEqual { Path = "back"
                     Cursor = 4
                     BackStack = ["back4", 5]
                     ForwardStack = ["back2", 3; "back1", 2; "path", 1; "fwd", 6] }

[<Test>]
let ``Back 3 as overkill will not throw``() =
    backCount 3 ["back", 2] []
    |> shouldEqual { Path = "back"
                     Cursor = 2
                     BackStack = []
                     ForwardStack = ["path", 1] }

[<Test>]
let ``Forward 3 will move 3 times``() =
    forwardCount 3 ["back", 2] ["fwd1", 3; "fwd2", 4; "fwd", 5; "fwd4", 6]
    |> shouldEqual { Path = "fwd"
                     Cursor = 5
                     BackStack = ["fwd2", 4; "fwd1", 3; "path", 1; "back", 2]
                     ForwardStack = ["fwd4", 6] }

[<Test>]
let ``Forward 3 as too far will not throw``() =
    forwardCount 3 [] ["fwd", 2]
    |> shouldEqual { Path = "fwd"
                     Cursor = 2
                     BackStack = ["path", 1]
                     ForwardStack = [] }
