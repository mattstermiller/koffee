module Koffee.MainNavTests_SuggestPaths

open NUnit.Framework
open FsUnitTyped
open FSharp.Control
open Acadian.FSharp

let fs = FakeFileSystem [
    folder "old projects" []
    folder "programs" []
    folder "Projects" []
    folder "temporary" []
]

let folders = fs.ItemsIn "/c" |> List.map (fun i -> { PathValue = i.Path; IsDirectory = true })

let history =
    folders @ (
        [
            "/c/users/me/my programs/"
            "/c/programs/vim/"
            "/c/programs/grammar wizard/"
        ] |> List.map createHistoryPath
    )

let suggestPaths fsReader model =
    Main.Nav.suggestPaths fsReader model
    |> AsyncSeq.lastOrDefault model
    |> Async.RunSynchronously

let testCases () =
    [   ("/c", [])
        ("/c/", folders)
        ("/c/ ", folders)
        ("/c/old", [ folders.[0] ])
        ("/c/OLD", [ folders.[0] ])
        ("/c/old ", [ folders.[0] ])
        ("/c/ old", [ folders.[0] ])
        ("/c/ro", [
            folders.[0]
            folders.[1]
            folders.[2]
        ])
        ("/c/pro", [
            folders.[1]
            folders.[2]
            folders.[0]
        ])
        ("/c/pro am", [ folders.[1] ])
        ("/c/e s", [
            folders.[0]
            folders.[2]
        ])
        ("", [])
        ("gram", [
            history.[6]
            history.[1]
            history.[4]
        ])
        ("invalid/gram", [])
    ] |> List.map (fun (inp, exp) -> TestCaseData(inp, exp))

[<TestCaseSource(nameof testCases)>]
let ``Path suggestions return expected results`` input expected =
    let model =
        { testModel with
            LocationInput = input
            History = { testModel.History with Paths = history }
        }

    let actual = suggestPaths fs model

    actual.PathSuggestions |> shouldEqual (Ok expected)

[<Test>]
let ``Path suggestions return error`` () =
    let ex = exn ""
    let fs = FakeFileSystem []
    fs.AddExn false ex "/c"
    let model = { testModel with LocationInput = "/c/" }

    let actual = suggestPaths fs model

    actual.PathSuggestions |> shouldEqual (Error ex.Message)

[<Test>]
let ``Path suggestions cache folder listings`` () =
    let fs = FakeFileSystem []
    let model =
        { testModel with
            LocationInput = "/c/"
            PathSuggestCache = Some (createPath "/c/", Ok (folders |> List.map (fun hp -> hp.PathValue)))
        }

    let cacheHit = suggestPaths fs model
    fs.CallsToGetItems |> shouldEqual 0
    cacheHit |> shouldEqual { model with PathSuggestions = Ok folders }

    let missModel = { model with LocationInput = "/c/path/" }
    let cacheMiss = suggestPaths fs missModel
    fs.CallsToGetItems |> shouldEqual 1
    cacheMiss
    |> shouldEqual
        { missModel with
            PathSuggestions = Ok []
            PathSuggestCache = Some (createPath "/c/path/", Ok [])
        }
