module Koffee.HistoryTests

open NUnit.Framework
open FsUnitTyped
open Newtonsoft.Json

[<Test>]
let ``History can be serialized and deserialized`` () =
    let history =
        { History.Default with
            PathSort = Map.ofList [
                (@"C:\Some\Path", {Sort = SortField.Name; Descending = true})
                (@"C:\Some\Other\Path", {Sort = SortField.Modified; Descending = false})
                (@"C:\Some\Third\Path", {Sort = SortField.Size; Descending = false})
            ]
            NetHosts = [
                "Some net host"
            ]
            Paths = [
                (Path.Parse "C:\Some\Path").Value
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
    let path = (Path.Parse "C:\Some\Path").Value
    let pathKey = path.Format Windows
    let sort = { Sort = SortField.Modified; Descending = true }
    let history = {
        History.Default with
            PathSort = Map.empty
    }

    // Act
    let result = history.WithPathSort path sort
    let pathsInHistory = result.PathSort |> Map.toSeq |> Seq.map fst

    // Assert
    pathsInHistory |> shouldContain pathKey

[<Test>]
let ``With same path in PathSort it overrides the existing`` () =
    // Arrange
    let path = (Path.Parse "C:\Some\Path").Value
    let pathKey = path.Format Windows
    let sort = { Sort = SortField.Modified; Descending = true }
    let history = {
        History.Default with
            PathSort = Map.ofList [
                (pathKey, sort)
            ]
    }
    let newSort = { Sort = SortField.Size; Descending = true }

    // Act
    let result = history.WithPathSort path newSort
    let resultSort = result.PathSort.[pathKey]

    // Assert
    resultSort |> shouldEqual newSort
    resultSort |> shouldNotEqual sort

[<Test>]
let ``With ascending name sort it omits it`` () =
    // Arrange
    let path = (Path.Parse "C:\Some\Path").Value
    let pathKey = path.Format Windows
    let sort = { Sort = SortField.Name; Descending = false }
    let history = History.Default

    // Act
    let result = history.WithPathSort path sort
    let pathsInHistory = result.PathSort |> Map.toSeq |> Seq.map fst

    // Assert
    pathsInHistory |> shouldNotContain pathKey

[<Test>]
let ``With ascending name sort it removes the existing`` () =
    // Arrange
    let path = (Path.Parse "C:\Some\Path").Value
    let pathKey = path.Format Windows
    let sort = { Sort = SortField.Modified; Descending = true }
    let history = {
        History.Default with
            PathSort = Map.ofList [
                (pathKey, sort)
            ]
    }
    let newSort = { Sort = SortField.Name; Descending = false }

    // Act
    let result = history.WithPathSort path newSort
    let pathsInHistory = result.PathSort |> Map.toSeq |> Seq.map fst

    // Assert
    pathsInHistory |> shouldNotContain pathKey

