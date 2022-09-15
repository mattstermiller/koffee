module Koffee.MainLogicTests_InitModel

open NUnit.Framework
open FsUnitTyped
open Acadian.FSharp

type TestResult = {
    Start: string
    Back: string option
    Error: string option
}

let test start path1 path2 =
    let invalid, valid =
        [path1; path2] @ (start |> Option.toList)
        |> List.partition (String.contains "invalid")
    let fs = FakeFileSystem (valid |> List.map (fun p -> folder p []))
    for p in invalid do
        fs.AddExn false (exn p) p
    let config = { Config.Default with DefaultPath = createPath path2; PathFormat = Unix }
    let history = { History.Default with Paths = [createPath path1] }
    let model = { MainModel.Default with Config = config; History = history }
    let options = { StartPath = start; StartLocation = None; StartSize = None }
    let screen = Rect.ofPairs (0, 0) (800, 600)

    let actual = MainLogic.initModel fs screen options model

    actual.Items |> shouldNotEqual [Item.Empty]
    { Start = actual.LocationFormatted
      Back = actual.BackStack |> List.tryHead |> Option.map (fun (p, _) -> p.FormatFolder Unix)
      Error =
          match actual.Status with
          | Some (ErrorMessage e) -> Some e
          | _ -> None
    }

let openError p = (ActionError ("open path", exn p)).Message

[<Test>]
let ``When all paths are valid then opens start and back is first path`` () =
    test (Some "/c/start/") "/c/path1" "/c/path2"
    |> shouldEqual { Start = "/c/start/"; Back = Some "/c/path1/"; Error = None }

[<TestCase(false)>]
[<TestCase(true)>]
let ``When start is invalid or missing and both paths are valid then opens first path and back is second`` isStartInvalid =
    let start, expectedError =
        if isStartInvalid then (Some "/c/invalid/", Some (openError "/c/invalid/"))
        else (None, None)
    test start "/c/path1" "/c/path2"
    |> shouldEqual { Start = "/c/path1/"; Back = Some "/c/path2/"; Error = expectedError }

[<Test>]
let ``When first path is invalid then opens start and back is first`` () =
    test (Some "/c/start/") "/c/invalid" "/c/path2"
    |> shouldEqual { Start = "/c/start/"; Back = Some "/c/invalid/"; Error = None }

[<Test>]
let ``When all paths are invalid then opens root and back is empty`` () =
    test (Some "/c/invalid/") "/c/invalid1" "/c/invalid2"
    |> shouldEqual { Start = "/"; Back = None; Error = Some (openError "/c/invalid/") }

[<Test>]
let ``When start is same as first path then opens start and back is second`` () =
    test (Some "/c/start") "/c/Start" "/c/path2"
    |> shouldEqual { Start = "/c/start/"; Back = Some "/c/path2/"; Error = None }

[<Test>]
let ``When start is same as both paths then opens start and back is root`` () =
    test (Some "/c/start") "/c/Start" "/c/START"
    |> shouldEqual { Start = "/c/start/"; Back = Some "/"; Error = None }
