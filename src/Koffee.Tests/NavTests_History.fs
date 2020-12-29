module Koffee.NavTests_History

open FsUnitTyped
open Koffee.MainLogic
open NUnit.Framework

let historyWithPathSort location sort =
    { History.Default with
        PathSort = Map.empty |> Map.add location sort
    }

[<Test>]
let ``Sorting changes stored sort`` () =
    let location = createPath "/c/sample"
    let originalPathSort = { Sort = Modified; Descending = true }
    let model =
        { MainModel.Default with
            Sort = Some (originalPathSort.Sort, originalPathSort.Descending)
            History = historyWithPathSort location originalPathSort
        }.WithLocation location

    let resultModel = Nav.sortList Name model

    let resultSort = resultModel.History.PathSort.TryFind location
    resultSort |> shouldNotEqual (Some originalPathSort)

[<Test>]
let ``Sorting on different folder leaves stored sort unchanged`` () =
    let modelLocation = createPath "/c/sample2"
    let pathSortLocation = createPath "/c/sample1"
    let originalPathSort = { Sort = Modified; Descending = true }
    let model =
        { MainModel.Default with
            Sort = Some (originalPathSort.Sort, originalPathSort.Descending)
            History = historyWithPathSort pathSortLocation originalPathSort
        }.WithLocation modelLocation

    let resultModel = Nav.sortList Name model

    let resultSort = resultModel.History.PathSort.TryFind pathSortLocation
    resultSort |> shouldEqual (Some originalPathSort)

[<Test>]
let ``Toggling sort into default removes it from history`` () =
    let expected = None
    let location = createPath "/c/sample"
    let originalPathSort = { PathSort.Default with Descending = not PathSort.Default.Descending }
    let model =
        { MainModel.Default with
            Sort = Some (originalPathSort.Sort, originalPathSort.Descending)
            History = historyWithPathSort location originalPathSort
        }.WithLocation location

    let resultModel = Nav.sortList PathSort.Default.Sort model

    let resultSort = resultModel.History.PathSort.TryFind location
    resultSort |> shouldEqual expected

let createFs () =
    FakeFileSystem [
        folder "programs" [
            file "koffee.exe"
            file "notepad.exe"
        ]
        file "readme.md"
    ]

[<Test>]
let ``Use stored sort on path change`` () =
    let location = createPath "/c/"
    let originalPathSort = PathSort.Default
    let storedLocation = createPath "/c/programs"
    let storedPathSort = { Sort = Modified; Descending = true }
    let expected = (storedPathSort.Sort, storedPathSort.Descending)
    let model =
        { MainModel.Default with
            Sort = Some (originalPathSort.Sort, originalPathSort.Descending)
            History = historyWithPathSort storedLocation storedPathSort
        }.WithLocation location
    let fs = createFs()

    let result = Nav.openPath fs storedLocation SelectNone model

    let resultModel = assertOk result
    resultModel.Sort |> shouldEqual (Some expected)

[<Test>]
let ``Use default sort on path change when no stored sort`` () =
    let expected = (PathSort.Default.Sort, PathSort.Default.Descending)
    let location = createPath "/c/"
    let newLocation = createPath "/c/programs"
    let originalPathSort = PathSort.Default
    let model =
        { MainModel.Default with
            Sort = Some (originalPathSort.Sort, originalPathSort.Descending)
            History = History.Default
        }.WithLocation location
    let fs = createFs()

    let result = Nav.openPath fs newLocation SelectNone model

    let resultModel = assertOk result
    resultModel.Sort |> shouldEqual (Some expected)
