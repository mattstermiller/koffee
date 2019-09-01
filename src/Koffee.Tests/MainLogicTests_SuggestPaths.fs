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
    ] |> List.map (fun (inp, exp) -> TestCaseData(inp, exp))

[<TestCaseSource("testCases")>]
let ``Path suggestions return expected results`` input expected =
    let fsReader = FakeFileSystemReader()
    fsReader.GetFolders <- fun path ->
        if path = createPath "/c/" then Ok (folders |> List.map createNode) else Ok []
    let model = { baseModel with LocationInput = input }

    let actual = MainLogic.Nav.suggestPaths fsReader model
                 |> AsyncSeq.lastOrDefault model
                 |> Async.RunSynchronously

    actual.PathSuggestions |> shouldEqual (Ok expected)

[<Test>]
let ``Path suggestions return error`` () =
    let err = exn ""
    let fsReader = FakeFileSystemReader()
    fsReader.GetFolders <- fun path ->
        if path = createPath "/c/" then Error err else Ok []
    let model = { baseModel with LocationInput = "/c/" }

    let actual = MainLogic.Nav.suggestPaths fsReader model
                 |> AsyncSeq.lastOrDefault model
                 |> Async.RunSynchronously

    actual.PathSuggestions |> shouldEqual (Error err.Message)
