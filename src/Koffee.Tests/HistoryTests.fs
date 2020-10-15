module Koffee.HistoryTests

open NUnit.Framework
open FsUnitTyped
open Newtonsoft.Json

let parseForTest pathStr =
    match Path.Parse pathStr with
    | Some p -> p
    | None -> failwithf "Test path string '%s' does not parse" pathStr

[<Test>]
let ``History can be serialized and deserialized`` () =
    let history =
        { History.Default with
            PathSort = Map.ofList [
                (parseForTest @"C:\Some\Path", {Sort = SortField.Name; Descending = true})
                (parseForTest @"C:\Some\Other\Path", {Sort = SortField.Modified; Descending = false})
                (parseForTest @"C:\Some\Third\Path", {Sort = SortField.Size; Descending = false})
            ]
            NetHosts = [
                "Some net host"
            ]
            Paths = [
                parseForTest "C:\Some\Path"
            ]
            Searches = [
                ("downloads", false, false)
                ("images", false, false)
            ]
        }
    let converters = FSharpJsonConverters.getAll ()
    let serialized = JsonConvert.SerializeObject(history, Formatting.Indented, converters)
    let deserialized = JsonConvert.DeserializeObject<History>(serialized, converters)
    deserialized |> shouldEqual history

[<Test>]
let ``With new PathSort it adds it to the list`` () =
    // Arrange
    let path = parseForTest "C:\Some\Path"
    let sort = { Sort = SortField.Modified; Descending = true }
    let history = {
        History.Default with
            PathSort = Map.empty
    }

    // Act
    let result = history.WithPathSort path sort
    let pathsInHistory = result.PathSort |> Map.toSeq |> Seq.map fst

    // Assert
    pathsInHistory |> shouldContain path

[<Test>]
let ``With same path in PathSort it overrides the existing`` () =
    // Arrange
    let path = parseForTest "C:\Some\Path"
    let sort = { Sort = SortField.Modified; Descending = true }
    let history = {
        History.Default with
            PathSort = Map.ofList [
                (path, sort)
            ]
    }
    let newSort = { Sort = SortField.Size; Descending = true }

    // Act
    let result = history.WithPathSort path newSort
    let resultSort = result.PathSort.[path]

    // Assert
    resultSort |> shouldEqual newSort
    resultSort |> shouldNotEqual sort

[<Test>]
let ``With default sort it omits it`` () =
    // Arrange
    let path = parseForTest "C:\Some\Path"
    let sort = PathSort.Default
    let history = History.Default

    // Act
    let result = history.WithPathSort path sort
    let pathsInHistory = result.PathSort |> Map.toSeq |> Seq.map fst

    // Assert
    pathsInHistory |> shouldNotContain path

[<Test>]
let ``With default sort it removes the existing`` () =
    // Arrange
    let path = parseForTest "C:\Some\Path"
    let sort = { PathSort.Default with Descending = not PathSort.Default.Descending }
    let history = {
        History.Default with
            PathSort = Map.ofList [
                (path, sort)
            ]
    }
    let newSort = PathSort.Default

    // Act
    let result = history.WithPathSort path newSort
    let pathsInHistory = result.PathSort |> Map.toSeq |> Seq.map fst

    // Assert
    pathsInHistory |> shouldNotContain path

