module Koffee.ItemToolTests

open System
open NUnit.Framework
open FsUnitTyped
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

[<Test>]
let ``substituteArgumentVariables substitutes selected_files with selected items of file type`` () =
    let model =
        { testModel with
            Cursor = 5
            SelectedItems = testModel.Items |> List.take 5
        }
    Tools.substituteArgumentVariables model "--files {selected_files}"
    |> shouldEqual @"--files C:\file1 C:\file2"

[<Test>]
let ``substituteArgumentVariables substitutes selected_files with empty string when no items are files`` () =
    let model =
        { testModel with
            Cursor = 5
            SelectedItems = testModel.Items |> List.take 3
        }
    Tools.substituteArgumentVariables model "--files {selected_files}"
    |> shouldEqual "--files "

[<Test>]
let ``substituteArgumentVariables substitutes bracketed selected_files arg with empty string when no items are files`` () =
    let model =
        { testModel with
            Cursor = 5
            SelectedItems = testModel.Items |> List.take 3
        }
    Tools.substituteArgumentVariables model "[--files {selected_files}]"
    |> shouldEqual String.Empty
