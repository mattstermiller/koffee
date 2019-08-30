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
    let validPath (path: Path) =
        let path = path.Format Unix
        if not (path |> String.contains "invalid") then
            Ok ()
        else Error (exn path)
    let fsReader = FakeFileSystemReader()
    fsReader.GetNode <- validPath >> Result.map (cnst None)
    fsReader.GetNodes <- validPath >> Result.map (cnst [Node.Empty])
    let config = { Config.Default with PreviousPath = createPath path1; DefaultPath = createPath path2; PathFormat = Unix }
    let options = { StartPath = start; StartLocation = None; StartSize = None }
    let model = { MainModel.Default with Config = config } |> MainLogic.initModel fsReader options
    { Start = model.LocationFormatted
      Back = model.BackStack |> List.tryHead |> Option.map (fun (p, _) -> p.Format Unix)
      Error =
          match model.Status with
          | Some (ErrorMessage e) -> Some e
          | _ -> None
    }

let openError p = (ActionError ("open path", exn p)).Message

[<Test>]
let ``When all paths are valid, opens start and back is first path`` () =
    test (Some "/c/start") "/c/path1" "/c/path2"
    |> shouldEqual { Start = "/c/start"; Back = Some "/c/path1"; Error = None }

[<TestCase(false)>]
[<TestCase(true)>]
let ``When start is invalid or missing and both paths are valid, opens first path and back is second`` isStartInvalid =
    let start, expectedError =
        if isStartInvalid then (Some "/c/invalid", Some (openError "/c/invalid"))
        else (None, None)
    test start "/c/path1" "/c/path2"
    |> shouldEqual { Start = "/c/path1"; Back = Some "/c/path2"; Error = expectedError }

[<Test>]
let ``When first path is invalid, opens start and back is first`` () =
    test (Some "/c/start") "/c/invalid" "/c/path2"
    |> shouldEqual { Start = "/c/start"; Back = Some "/c/invalid"; Error = None }

[<Test>]
let ``When all paths are invalid, opens root and back is empty`` () =
    test (Some "/c/invalid") "/c/invalid1" "/c/invalid2"
    |> shouldEqual { Start = "/"; Back = None; Error = Some (openError "/c/invalid") }
