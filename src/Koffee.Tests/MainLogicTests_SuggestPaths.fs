module Koffee.MainLogicTests_SuggestPaths

open NUnit.Framework
open FsUnitTyped
open Acadian.FSharp
open FSharp.Control

let folders = [
    "/c/old projects/"
    "/c/programs/"
    "/c/Projects/"
    "/c/temporary/"
]

let history = folders @ [
    "/c/users/me/my programs/"
    "/c/programs/vim/"
    "/c/programs/grammar wizard/"
]

let suggestPaths fsReader model =
    MainLogic.Nav.suggestPaths fsReader model
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
            history.[4]
            history.[1]
        ])
        ("invalid/gram", [])
    ] |> List.map (fun (inp, exp) -> TestCaseData(inp, exp))

[<TestCaseSource("testCases")>]
let ``Path suggestions return expected results`` input expected =
    let fsReader = FakeFileSystemReader()
    fsReader.GetFolders <- fun path ->
        if path = createPath "/c/" then Ok (folders |> List.map createItem) else Ok []
    let model =
        { baseModel with
            LocationInput = input
            History = { baseModel.History with Paths = history |> List.map createPath }
        }

    let actual = suggestPaths fsReader model
    actual.PathSuggestions |> shouldEqual (Ok expected)

[<Test>]
let ``Path suggestions return error`` () =
    let err = exn ""
    let fsReader = FakeFileSystemReader()
    fsReader.GetFolders <- fun path ->
        if path = createPath "/c/" then Error err else Ok []
    let model = { baseModel with LocationInput = "/c/" }

    let actual = suggestPaths fsReader model
    actual.PathSuggestions |> shouldEqual (Error err.Message)

[<Test>]
let ``Path suggestions cache folder listings`` () =
    let mutable fsCalls = 0
    let fsReader = FakeFileSystemReader()
    fsReader.GetFolders <- fun _ ->
        fsCalls <- fsCalls + 1
        Ok []
    let model =
        { baseModel with
            LocationInput = "/c/"
            PathSuggestCache = Some (createPath "/c/", Ok (folders |> List.map createPath))
        }

    let cacheHit = suggestPaths fsReader model
    fsCalls |> shouldEqual 0
    cacheHit |> shouldEqual { model with PathSuggestions = Ok folders }

    let missModel = { model with LocationInput = "/c/path/" }
    let cacheMiss = suggestPaths fsReader missModel
    fsCalls |> shouldEqual 1
    let expected =
        { missModel with
            PathSuggestions = Ok []
            PathSuggestCache = Some (createPath "/c/path/", Ok [])
        }
    cacheMiss |> shouldEqual expected
