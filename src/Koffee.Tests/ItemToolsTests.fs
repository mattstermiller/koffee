module Koffee.ItemToolTests

open NUnit.Framework
open FsUnitTyped
open Acadian.FSharp
open Koffee.ItemActionCommands

let testModel =
    let items = [
        createFolder "/c/folder1"
        createFolder "/c/folder2"
        createFolder "/c/folder3"
        createFile "/c/file1"
        createFile "/c/file2"
        createFile "/c/file3"
    ]
    { testModel with
        Directory = items
        Items = items
    }

let fs = FakeFileSystem []
let substituteVariables = Tools.substituteVariables fs (cnst None) "Tool"

let variableRequired varName = MainStatus.ToolVariableRequired ("Tool", varName)

// selected variables

[<Test>]
let ``substituteVariables selected_items substitutes with selected item paths`` () =
    let model =
        { testModel with
            Cursor = 5
            SelectedItems = testModel.Items |> List.take 5
        }
    substituteVariables model "--select {selected_items}"
    |> assertAreEqual (Ok {
        Value = @"--select ""C:\folder1"" ""C:\folder2"" ""C:\folder3"" ""C:\file1"" ""C:\file2"""
        PathsUsed = model.SelectedItems |> List.map (fun item -> item.Path)
    })

[<TestCase(",")>]
[<TestCase(":")>]
[<TestCase(" --select ")>]
let ``substituteVariables selected_items with custom separator substitutes with selected item paths`` separator =
    let model =
        { testModel with
            Cursor = 5
            SelectedItems = testModel.Items |> List.take 3
        }
    let expected =
        [
            @"""C:\folder1"""
            @"""C:\folder2"""
            @"""C:\folder3"""
        ]
        |> String.concat separator
    substituteVariables model (sprintf "--select {selected_items:%s}" separator)
    |> assertAreEqual (Ok {
        Value = "--select " + expected
        PathsUsed = model.SelectedItems |> List.map (fun item -> item.Path)
    })

[<Test>]
let ``substituteVariables selected_files substitutes with selected item paths of file type`` () =
    let model =
        { testModel with
            Cursor = 5
            SelectedItems = testModel.Items |> List.take 5
        }
    substituteVariables model "--files {selected_files}"
    |> assertAreEqual (Ok {
        Value = @"--files ""C:\file1"" ""C:\file2"""
        PathsUsed = ["/c/file1"; "/c/file2"] |> List.map createPath
    })

[<Test>]
let ``substituteVariables selected_files when no items are files returns variable required error`` () =
    let model =
        { testModel with
            Cursor = 5
            SelectedItems = testModel.Items |> List.take 3
        }
    substituteVariables model "--files {selected_files}"
    |> assertAreEqual (Error (variableRequired (SelectedFiles None)))

[<Test>]
let ``substituteVariables selected_files bracketed substitutes with selected item paths of file type`` () =
    let model =
        { testModel with
            Cursor = 5
            SelectedItems = testModel.Items |> List.take 5
        }
    substituteVariables model "--files {selected_files}"
    |> assertAreEqual (Ok {
        Value = @"--files ""C:\file1"" ""C:\file2"""
        PathsUsed = ["/c/file1"; "/c/file2"] |> List.map createPath
    })

[<Test>]
let ``substituteVariables selected_files bracketed when no items are files substitutes arg with empty string`` () =
    let model =
        { testModel with
            Cursor = 5
            SelectedItems = testModel.Items |> List.take 3
        }
    substituteVariables model "--test [--files {selected_files}]"
    |> assertAreEqual (Ok {
        Value = "--test "
        PathsUsed = []
    })

[<Test>]
let ``substituteVariables selected_folders substitutes with selected item paths of folder type`` () =
    let model =
        { testModel with
            Cursor = 5
            SelectedItems = testModel.Items |> List.take 5
        }
    substituteVariables model "--folders {selected_folders}"
    |> assertAreEqual (Ok {
        Value = @"--folders ""C:\folder1"" ""C:\folder2"" ""C:\folder3"""
        PathsUsed = ["/c/folder1"; "/c/folder2"; "/c/folder3"] |> List.map createPath
    })

[<Test>]
let ``substituteVariables selected_items with no selection substitutes with cursor item`` () =
    let model =
        { testModel with
            Cursor = 5
        }
    substituteVariables model "--select {selected_items}"
    |> assertAreEqual (Ok {
        Value = @"--select ""C:\file3"""
        PathsUsed = model.CursorItem |> Option.map (fun item -> item.Path) |> Option.toList
    })

// cursor variables

[<Test>]
let ``substituteVariables cursor_item substitutes with cursor item path`` () =
    let model =
        { testModel with
            Cursor = 5
            SelectedItems = testModel.Items |> List.take 2
        }
    substituteVariables model "--select {cursor_item}"
    |> assertAreEqual (Ok {
        Value = @"--select ""C:\file3"""
        PathsUsed = [createPath "/c/file3"]
    })

[<Test>]
let ``substituteVariables cursor_file on file substitutes with cursor item path`` () =
    let model =
        { testModel with
            Cursor = 5
            SelectedItems = testModel.Items |> List.take 4
        }
    substituteVariables model "--select {cursor_file}"
    |> assertAreEqual (Ok {
        Value = @"--select ""C:\file3"""
        PathsUsed = [createPath "/c/file3"]
    })

[<Test>]
let ``substituteVariables cursor_file on folder returns variable required error`` () =
    let model =
        { testModel with
            Cursor = 1
            SelectedItems = testModel.Items |> List.take 4
        }
    substituteVariables model "--select {cursor_file}"
    |> assertAreEqual (Error (variableRequired CursorFile))

[<Test>]
let ``substituteVariables cursor_folder on folder substitutes with cursor item path`` () =
    let model =
        { testModel with
            Cursor = 1
            SelectedItems = testModel.Items.[2..4]
        }
    substituteVariables model "--select {cursor_folder}"
    |> assertAreEqual (Ok {
        Value = @"--select ""C:\folder2"""
        PathsUsed = [createPath "/c/folder2"]
    })

[<Test>]
let ``substituteVariables cursor_folder on file returns variable required error`` () =
    let model =
        { testModel with
            Cursor = 5
            SelectedItems = testModel.Items.[2..4]
        }
    substituteVariables model "--select {cursor_folder}"
    |> assertAreEqual (Error (variableRequired CursorFolder))

// other variables

[<Test>]
let ``substituteVariables location substitutes with location path`` () =
    substituteVariables testModel "--folder {location}"
    |> assertAreEqual (Ok {
        Value = @"--folder ""C:\"""
        PathsUsed = []
    })

[<Test>]
let ``substituteVariables git_root substitutes with path of parent containing .git`` () =
    let fs = FakeFileSystem [
        folder "other" []
        folder "projects" [
            folder ".git" []
            folder "banana" [
                folder ".git" []
                folder "src" [
                    folder "app" []
                ]
            ]
        ]
    ]
    let model = testModel |> withLocation "/c/projects/banana/src/app"
    Tools.substituteVariables fs (cnst None) "Tool" model "--folder {git_root}"
    |> assertAreEqual (Ok {
        Value = @"--folder ""C:\projects\banana"""
        PathsUsed = []
    })

[<Test>]
let ``substituteVariables git_root with no .git in parents return variable required error`` () =
    let fs = FakeFileSystem [
        folder "other" [
            folder ".git" []
        ]
        folder "projects" [
            folder "banana" [
                folder "src" [
                    folder "app" []
                ]
            ]
        ]
    ]
    let model = testModel |> withLocation "/c/projects/banana/src/app"
    Tools.substituteVariables fs (cnst None) "Tool" model "--folder {git_root}"
    |> assertAreEqual (Error (variableRequired GitRoot))

[<Test>]
let ``substituteVariables env substitutes with environment variable`` () =
    let getEnvVar varName = Some (varName + "_VALUE")
    Tools.substituteVariables fs getEnvVar "Tool" testModel "--edit {env:EDITOR}"
    |> assertAreEqual (Ok {
        Value = @"--edit ""EDITOR_VALUE"""
        PathsUsed = []
    })

[<Test>]
let ``substituteVariables env with undefined environment variable returns variable required error`` () =
    Tools.substituteVariables fs (cnst None) "Tool" testModel "--edit {env:EDITOR}"
    |> assertAreEqual (Error (variableRequired (Env "EDITOR")))

// variable fallback

[<TestCase(0)>]
[<TestCase(1)>]
[<TestCase(2)>]
let ``substituteVariables with fallback substitutes with variable that has value`` fallbacks =
    let getEnvVar varName =
        if varName = "TOOL_PATH" && fallbacks = 0
        then Some @"C:\tool"
        else None
    let model =
        { testModel with
            Cursor = 5
            SelectedItems = if fallbacks = 1 then testModel.Items |> List.take 2 else []
        }

    Tools.substituteVariables fs getEnvVar "Tool" model "{env:TOOL_PATH?selected_folders:,?location}"
    |> assertAreEqual (Ok (
        match fallbacks with
        | 0 ->
            {
                Value = @"""C:\tool"""
                PathsUsed = []
            }
        | 1 ->
            {
                Value = @"""C:\folder1"",""C:\folder2"""
                PathsUsed = ["/c/folder1"; "/c/folder2"] |> List.map createPath
            }
        | _ ->
            {
                Value = @"""C:\"""
                PathsUsed = []
            }
    ))

// multiple variables

[<Test>]
let ``substituteVariables with multiple variables substitutes correctly`` () =
    let model =
        { testModel with
            Cursor = 5
            SelectedItems = testModel.Items |> List.take 2
        }
    substituteVariables model "--path {location} [--select {selected_items}] --current {cursor_item}"
    |> assertAreEqual (Ok {
        Value = @"--path ""C:\"" --select ""C:\folder1"" ""C:\folder2"" --current ""C:\file3"""
        PathsUsed = [0; 1; 5] |> List.map (fun i -> model.Items.[i].Path)
    })
