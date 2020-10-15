module Koffee.NavTests_History

open FsUnitTyped
open Koffee.MainLogic
open NUnit.Framework

let newPath pathStr =
    match Path.Parse pathStr with
    | Some p -> p
    | None -> failwithf "Test path string '%s' does not parse" pathStr

let historyWithPathSort location sort =
    { History.Default with 
        PathSort = Map.empty |> Map.add location sort
    }

[<Test>]
let ``Sorting changes stored sort`` () =
    // Arrange
    let location = newPath "C:/Sample"
    let originalPathSort = { Sort = Modified; Descending = true }
    let model =
        { MainModel.Default with 
            Sort = Some (originalPathSort.Sort, originalPathSort.Descending)
            History = historyWithPathSort location originalPathSort
        }.WithLocation location

    // Act
    let resultModel = Nav.sortList Name model

    // Assert
    let resultSort = resultModel.History.PathSort.TryFind location
    resultSort |> shouldNotEqual (Some originalPathSort)

[<Test>]
let ``Sorting on different folder leaves stored sort unchanged`` () =
    // Arrange
    let modelLocation = newPath "C:/Sample2"
    let pathSortLocation = newPath "C:/Sample1"
    let originalPathSort = { Sort = Modified; Descending = true }
    let model =
        { MainModel.Default with 
            Sort = Some (originalPathSort.Sort, originalPathSort.Descending)
            History = historyWithPathSort pathSortLocation originalPathSort
        }.WithLocation modelLocation

    // Act
    let resultModel = Nav.sortList Name model

    // Assert
    let resultSort = resultModel.History.PathSort.TryFind pathSortLocation
    resultSort |> shouldEqual (Some originalPathSort)

[<Test>]
let ``Toggling sort into default removes it from history`` () =
    // Arrange
    let expected = None
    let location = newPath "C:/Sample"
    let originalPathSort = { PathSort.Default with Descending = not PathSort.Default.Descending }
    let model =
        { MainModel.Default with 
            Sort = Some (originalPathSort.Sort, originalPathSort.Descending)
            History = historyWithPathSort location originalPathSort
        }.WithLocation location

    // Act
    let resultModel = Nav.sortList PathSort.Default.Sort model

    // Assert
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

let expectOk onError = function
    | Error _ -> failwith onError
    | Ok result -> result

[<Test>]
let ``Use stored sort on path change`` () =
    // Arrange
    let location = newPath "/c/"
    let originalPathSort = PathSort.Default
    let storedLocation = newPath "/c/programs"
    let storedPathSort = { Sort = Modified; Descending = true }
    let expected = (storedPathSort.Sort, storedPathSort.Descending)
    let model =
        { MainModel.Default with 
            Sort = Some (originalPathSort.Sort, originalPathSort.Descending)
            History = historyWithPathSort storedLocation storedPathSort
        }.WithLocation location
    let fs = createFs()

    // Act
    let result = Nav.openPath fs storedLocation SelectNone model

    // Assert
    let resultModel = expectOk "Failed to open path" result
    resultModel.Sort |> shouldEqual (Some expected)

[<Test>]
let ``Use default sort on path change when no stored sort`` () =
    // Arrange
    let expected = (PathSort.Default.Sort, PathSort.Default.Descending)
    let location = newPath "/c/"
    let newLocation = newPath "/c/programs"
    let originalPathSort = PathSort.Default
    let model =
        { MainModel.Default with 
            Sort = Some (originalPathSort.Sort, originalPathSort.Descending)
            History = History.Default
        }.WithLocation location
    let fs = createFs()

    // Act
    let result = Nav.openPath fs newLocation SelectNone model

    // Assert
    let resultModel = expectOk "Failed to open path" result
    resultModel.Sort |> shouldEqual (Some expected)

