module Koffee.MainLogicTests_InitModel

open NUnit.Framework
open FsUnitTyped
open Acadian.FSharp

type TestResult = {
    Start: string
    Back: string option
    Error: MainStatus.Error option
}

let test (startPath: string option) configPath (defaultPath: string) (history: string list) =
    let errorPaths, goodPaths =
        [
            yield! history
            defaultPath
            yield! startPath |> Option.toList
        ]
        |> List.partition (String.contains "error")
    let fs = FakeFileSystem (goodPaths |> List.map (fun p -> folder p []))
    for p in errorPaths do
        fs.AddExn false (exn p) p
    let config =
        { Config.Default with
            StartPath = configPath
            DefaultPath = defaultPath |> createPath
            PathFormat = Unix
        }
    let history = { History.Default with Paths = history |> List.map createPath }
    let model = { MainModel.Default with Config = config; History = history }
    let options = { StartPath = startPath; StartLocation = None; StartSize = None }
    let screen = Rect.ofPairs (0, 0) (800, 600)

    let actual = MainLogic.initModel fs screen options model

    actual.Items |> shouldNotEqual [Item.Empty]
    { Start = actual.LocationFormatted
      Back = actual.BackStack |> List.tryHead |> Option.map (fun (p, _) -> p.FormatFolder Unix)
      Error =
          match actual.Status with
          | Some (MainStatus.Error e) -> Some e
          | _ -> None
    }

let openPathError p = MainStatus.ActionError ("open path", exn p)

[<Test>]
let ``When all paths are good then opens start and back is prev`` () =
    test (Some "/c/start/") DefaultPath "/c/default" ["/c/prev"]
    |> assertAreEqual { Start = "/c/start/"; Back = Some "/c/prev/"; Error = None }

[<TestCase(false)>]
[<TestCase(true)>]
let ``When start path errors or is not provided then opens default and back is prev`` isStartError =
    let start, expectedError =
        if isStartError
        then (Some "/c/error/", Some (openPathError "/c/error/"))
        else (None, None)
    test start DefaultPath "/c/default" ["/c/prev"]
    |> assertAreEqual { Start = "/c/default/"; Back = Some "/c/prev/"; Error = expectedError }

[<TestCase(false)>]
[<TestCase(true)>]
let ``When start path errors with restore prev then opens prev if some else default and back is empty`` hasPrev =
    let history, expectedStart =
        if hasPrev
        then (["/c/prev"], "/c/prev/")
        else ([], "/c/default/")
    test (Some "/c/start error/") RestorePrevious "/c/default" history
    |> assertAreEqual { Start = expectedStart; Back = None; Error = Some (openPathError "/c/start error/") }

[<TestCase(false)>]
[<TestCase(true)>]
let ``When default path errors or is not configured then opens prev and back is prev2`` isDefaultError =
    let configPath, defaultPath, expectedError =
        if isDefaultError
        then (DefaultPath, "/c/error/", Some (openPathError "/c/error/"))
        else (RestorePrevious, "/c/default", None)
    test None configPath defaultPath ["/c/prev"; "/c/prev2"]
    |> assertAreEqual { Start = "/c/prev/"; Back = Some "/c/prev2/"; Error = expectedError }

[<Test>]
let ``When default path errors and no history then opens root and back is empty`` () =
    test None DefaultPath "/c/default error/" []
    |> assertAreEqual { Start = "/"; Back = None; Error = Some (openPathError "/c/default error/") }

[<Test>]
let ``When prev path errors then opens default and back is prev`` () =
    test None RestorePrevious "/c/default/" ["/c/prev error"; "/c/prev2"]
    |> assertAreEqual { Start = "/c/default/"; Back = Some "/c/prev error/"; Error = Some (openPathError "/c/prev error") }

[<Test>]
let ``When all paths error then opens root and back is prev`` () =
    test (Some "/c/start error/") DefaultPath "/c/default error" ["/c/prev error"; "/c/prev2"]
    |> assertAreEqual { Start = "/"; Back = Some "/c/prev error/"; Error = Some (openPathError "/c/start error/") }

[<Test>]
let ``When start is same as prev path then opens start and back is prev2`` () =
    test (Some "/c/start") DefaultPath "/c/default" ["/c/Start"; "/c/prev2/"]
    |> assertAreEqual { Start = "/c/start/"; Back = Some "/c/prev2/"; Error = None }

[<Test>]
let ``When default is same as prev path then opens default and back is prev2`` () =
    test None DefaultPath "/c/default" ["/c/Default"; "/c/prev2"]
    |> assertAreEqual { Start = "/c/default/"; Back = Some "/c/prev2/"; Error = None }
