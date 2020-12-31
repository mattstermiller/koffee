module Koffee.HistoryTests

open NUnit.Framework
open FsUnitTyped
open Newtonsoft.Json

let seqSortPathsInHistory history =
    history.PathSort |> Map.toSeq |> Seq.map fst

[<Test>]
let ``History can be serialized and deserialized`` () =
    let history =
        { History.Default with
            PathSort = Map.ofList [
                (createPath @"C:\Some\Path", {Sort = SortField.Name; Descending = true})
                (createPath @"C:\Some\Other\Path", {Sort = SortField.Modified; Descending = false})
                (createPath @"C:\Some\Third\Path", {Sort = SortField.Size; Descending = false})
            ]
            NetHosts = [
                "Some net host"
            ]
            Paths = [
                createPath @"C:\Some\Path"
            ]
            Searches = [
                ("downloads", false, false, false)
                ("images", false, false, false)
            ]
        }
    let converters = FSharpJsonConverters.getAll ()
    let serialized = JsonConvert.SerializeObject(history, Formatting.Indented, converters)
    let deserialized = JsonConvert.DeserializeObject<History>(serialized, converters)
    deserialized |> shouldEqual history

[<Test>]
let ``With new PathSort it adds it to the list`` () =
    let path = createPath @"C:\Some\Path"
    let sort = { Sort = SortField.Modified; Descending = true }
    let history = {
        History.Default with
            PathSort = Map.empty
    }

    let result = history.WithPathSort path sort

    let pathsInHistory = seqSortPathsInHistory result
    pathsInHistory |> shouldContain path

[<Test>]
let ``With same path in PathSort it overrides the existing`` () =
    let path = createPath @"C:\Some\Path"
    let sort = { Sort = SortField.Modified; Descending = true }
    let history = {
        History.Default with
            PathSort = Map.empty |> Map.add path sort
    }
    let newSort = { Sort = SortField.Size; Descending = true }

    let result = history.WithPathSort path newSort

    let resultSort = result.PathSort.[path]
    resultSort |> shouldEqual newSort
    resultSort |> shouldNotEqual sort

[<Test>]
let ``With default sort it omits it`` () =
    let path = createPath @"C:\Some\Path"
    let sort = PathSort.Default
    let history = History.Default

    let result = history.WithPathSort path sort

    let pathsInHistory = seqSortPathsInHistory result
    pathsInHistory |> shouldNotContain path

[<Test>]
let ``With default sort it removes the existing`` () =
    let path = createPath @"C:\Some\Path"
    let sort = { PathSort.Default with Descending = not PathSort.Default.Descending }
    let history = {
        History.Default with
            PathSort = Map.empty |> Map.add path sort
    }
    let newSort = PathSort.Default

    let result = history.WithPathSort path newSort

    let pathsInHistory = seqSortPathsInHistory result
    pathsInHistory |> shouldNotContain path

let ``Get stored sort from find`` () =
    let path = createPath @"C:\Some\Path"
    let sort = { PathSort.Default with Descending = not PathSort.Default.Descending }
    let history = {
        History.Default with
            PathSort = Map.empty |> Map.add path sort
    }

    let result = history.FindSortOrDefault path

    result |> shouldNotEqual PathSort.Default

let ``Get default sort when none stored`` () =
    let path = createPath @"C:\Some\Path"
    let history = History.Default

    let result = history.FindSortOrDefault path

    result |> shouldEqual PathSort.Default
