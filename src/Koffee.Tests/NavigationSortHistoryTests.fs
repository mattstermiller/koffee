module Koffee.NavigationSortHistoryTests

open FsUnitTyped
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
        }
        |> MainModel.withLocation location

    let resultModel = NavigationCommands.sortList Name model

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
        }
        |> MainModel.withLocation modelLocation

    let resultModel = NavigationCommands.sortList Name model

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
        }
        |> MainModel.withLocation location

    let resultModel = NavigationCommands.sortList PathSort.Default.Sort model

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
let ``openPath uses stored sort`` () =
    let location = createPath "/c/"
    let originalPathSort = PathSort.Default
    let storedLocation = createPath "/c/programs"
    let storedPathSort = { Sort = Modified; Descending = true }
    let expected = (storedPathSort.Sort, storedPathSort.Descending)
    let model =
        { MainModel.Default with
            Sort = Some (originalPathSort.Sort, originalPathSort.Descending)
            History = historyWithPathSort storedLocation storedPathSort
        }
        |> MainModel.withLocation location
    let fs = createFs()

    let result = NavigationCommands.openPath fs storedLocation CursorStay model

    let resultModel = assertOk result
    resultModel.Sort |> shouldEqual (Some expected)

[<Test>]
let ``openPath uses default sort when no stored sort`` () =
    let expected = (PathSort.Default.Sort, PathSort.Default.Descending)
    let location = createPath "/c/"
    let newLocation = createPath "/c/programs"
    let originalPathSort = PathSort.Default
    let model =
        { MainModel.Default with
            Sort = Some (originalPathSort.Sort, originalPathSort.Descending)
            History = History.Default
        }
        |> MainModel.withLocation location
    let fs = createFs()

    let result = NavigationCommands.openPath fs newLocation CursorStay model

    let resultModel = assertOk result
    resultModel.Sort |> shouldEqual (Some expected)
