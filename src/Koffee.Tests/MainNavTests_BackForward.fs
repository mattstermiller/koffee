module Koffee.MainNavTests_BackForward

open NUnit.Framework
open FsUnitTyped
open Koffee.Main

type TestResult = {
    Path: string
    Cursor: int
    BackStack: (string * int) list
    ForwardStack: (string * int) list
    Error: MainStatus.Error option
}

let testHistory handler count backStack forwardStack =
    let createHistoryItem (name, cursor) = (createPath ("/c/" + name), cursor)
    let model =
        { testModel with
            BackStack = backStack |> List.map createHistoryItem
            ForwardStack = forwardStack |> List.map createHistoryItem
            Cursor = 1
            RepeatCommand = count
        } |> withLocation "/c/path"
    let items = List.init 6 (sprintf "file%i" >> file)
    let fs = FakeFileSystem [
        folder "back" items
        folder "fwd" items
        folder "path" []
    ]
    fs.AddExn false ex "/c/error"
    fs.AddExn false ex "/c/error2"

    let actual = handler fs model

    let historyToNamePair (path: Path, c) = (path.Name, c)
    let actualError =
        match actual.Status with
        | Some (MainStatus.Error e) -> Some e
        | _ -> None

    { Path = actual.Location.Name
      Cursor = actual.Cursor
      BackStack = actual.BackStack |> List.map historyToNamePair
      ForwardStack = actual.ForwardStack |> List.map historyToNamePair
      Error = actualError
    }

let back = testHistory Nav.back None
let forward = testHistory Nav.forward None
let backCount count = testHistory Nav.back (Some count)
let forwardCount count = testHistory Nav.forward (Some count)

[<Test>]
let ``Back without history does nothing``() =
    back [] ["fwd", 2]
    |> shouldEqual {
        Path = "path"
        Cursor = 1
        BackStack = []
        ForwardStack = ["fwd", 2]
        Error = None
    }

[<Test>]
let ``Back with simple history changes path and stacks``() =
    back ["back", 2] []
    |> shouldEqual {
        Path = "back"
        Cursor = 2
        BackStack = []
        ForwardStack = ["path", 1]
        Error = None
    }

[<Test>]
let ``Back with more history changes path and stacks``() =
    back ["back", 2; "back2", 3] ["fwd", 4; "fwd2", 5]
    |> shouldEqual {
        Path = "back"
        Cursor = 2
        BackStack = ["back2", 3]
        ForwardStack = ["path", 1; "fwd", 4; "fwd2", 5]
        Error = None
    }

[<Test>]
let ``Back 3 will go to third path and shift 3 paths on stacks``() =
    backCount 3 ["back1", 2; "back2", 3; "back", 4; "back4", 5] ["fwd", 6]
    |> shouldEqual {
        Path = "back"
        Cursor = 4
        BackStack = ["back4", 5]
        ForwardStack = ["back2", 3; "back1", 2; "path", 1; "fwd", 6]
        Error = None
    }

[<Test>]
let ``Back past end of stack will go to end and not give error``() =
    backCount 3 ["back", 2] []
    |> shouldEqual {
        Path = "back"
        Cursor = 2
        BackStack = []
        ForwardStack = ["path", 1]
        Error = None
    }

[<Test>]
let ``Back to inaccessible path pops stack and gives error``() =
    back ["error", 2; "back2", 3] ["fwd", 4]
    |> shouldEqual {
        Path = "path"
        Cursor = 1
        BackStack = ["back2", 3]
        ForwardStack = ["fwd", 4]
        Error = Some (MainStatus.CouldNotOpenPath (createPath "/c/error", Unix, ex))
    }

[<Test>]
let ``Back 2 to inaccessible path removes from stack and gives error``() =
    backCount 2 ["error", 2; "error", 3; "back3", 4] ["fwd", 5]
    |> shouldEqual {
        Path = "path"
        Cursor = 1
        BackStack = ["error", 2; "back3", 4]
        ForwardStack = ["fwd", 5]
        Error = Some (MainStatus.CouldNotOpenPath (createPath "/c/error", Unix, ex))
    }

[<Test>]
let ``Back past end of stack to inaccessible path removes from stack and gives error``() =
    backCount 3 ["error", 2; "error", 3] ["fwd", 5]
    |> shouldEqual {
        Path = "path"
        Cursor = 1
        BackStack = ["error", 2]
        ForwardStack = ["fwd", 5]
        Error = Some (MainStatus.CouldNotOpenPath (createPath "/c/error", Unix, ex))
    }


[<Test>]
let ``Forward without history does nothing``() =
    forward ["back", 2] []
    |> shouldEqual {
        Path = "path"
        Cursor = 1
        BackStack = ["back", 2]
        ForwardStack = []
        Error = None
    }

[<Test>]
let ``Forward with simple history changes path and stacks``() =
    forward [] ["fwd", 2]
    |> shouldEqual {
        Path = "fwd"
        Cursor = 2
        BackStack = ["path", 1]
        ForwardStack = []
        Error = None
    }

[<Test>]
let ``Forward with more history changes path and stacks``() =
    forward ["back", 2; "back2", 3] ["fwd", 4; "fwd2", 5]
    |> shouldEqual {
        Path = "fwd"
        Cursor = 4
        BackStack = ["path", 1; "back", 2; "back2", 3]
        ForwardStack = ["fwd2", 5]
        Error = None
    }

[<Test>]
let ``Forward 3 will go to third path and shift 3 paths on stacks``() =
    forwardCount 3 ["back", 2] ["fwd1", 3; "fwd2", 4; "fwd", 5; "fwd4", 6]
    |> shouldEqual {
        Path = "fwd"
        Cursor = 5
        BackStack = ["fwd2", 4; "fwd1", 3; "path", 1; "back", 2]
        ForwardStack = ["fwd4", 6]
        Error = None
    }

[<Test>]
let ``Forward past end of stack will go to end and not give error``() =
    forwardCount 3 [] ["fwd", 2]
    |> shouldEqual {
        Path = "fwd"
        Cursor = 2
        BackStack = ["path", 1]
        ForwardStack = []
        Error = None
    }

[<Test>]
let ``Forward to inaccessible path pops stack and gives error``() =
    forward ["back", 2] ["error", 3; "fwd2", 4]
    |> shouldEqual {
        Path = "path"
        Cursor = 1
        BackStack = ["back", 2]
        ForwardStack = ["fwd2", 4]
        Error = Some (MainStatus.CouldNotOpenPath (createPath "/c/error", Unix, ex))
    }

[<Test>]
let ``Forward 2 to inaccessible path removes from stack and gives error``() =
    forwardCount 2 ["back", 2] ["error", 3; "error", 4; "fwd3", 5]
    |> shouldEqual {
        Path = "path"
        Cursor = 1
        BackStack = ["back", 2]
        ForwardStack = ["error", 3; "fwd3", 5]
        Error = Some (MainStatus.CouldNotOpenPath (createPath "/c/error", Unix, ex))
    }

[<Test>]
let ``Forward past end of stack to inaccessible path removes from stack and gives error``() =
    forwardCount 3 ["back", 2] ["error", 3; "error", 4]
    |> shouldEqual {
        Path = "path"
        Cursor = 1
        BackStack = ["back", 2]
        ForwardStack = ["error", 3]
        Error = Some (MainStatus.CouldNotOpenPath (createPath "/c/error", Unix, ex))
    }
