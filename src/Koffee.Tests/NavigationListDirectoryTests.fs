module Koffee.NavigationListDirectoryTests

open NUnit.Framework

[<TestCase(false)>]
[<TestCase(true)>]
let ``listDirectory populates items from directory and filters out hidden items when configured`` showHidden =
    let directory = [
        createFile "/c/file1"
        createFile "/c/file2" |> hide true
        createFile "/c/file3"
        createFile "/c/file4" |> hide true
    ]
    let model =
        { testModel with Directory = directory }
        |> MainModel.mapConfig (fun config -> { config with ShowHidden = showHidden})

    let actual = NavigationCommands.listDirectory CursorStay model

    let expectedItems = directory |> applyIf (not showHidden) (List.filter (fun i -> not i.IsHidden))
    let expected = { model with Items = expectedItems }
    assertAreEqual expected actual


type CursorAndSelectionCase = {
    CursorMove: CursorMoveType
    ExpectedCursor: int
    ExpectedSelectedPaths: string list option
}

let cursorAndSelectionDirectory = [
    createFile "/c/file1"
    createFile "/c/file2" |> hide true
    createFile "/c/file3"
    createFile "/c/file4" |> hide true
    createFile "/c/file5"
]
let cursorAndSelectionCases =
    [
        {
            CursorMove = CursorStay
            ExpectedCursor = 1
            ExpectedSelectedPaths = None
        }
        {
            CursorMove = CursorToIndex 2
            ExpectedCursor = 2
            ExpectedSelectedPaths = None
        }
        {
            CursorMove = CursorToPath (createPath "/c/file5", false)
            ExpectedCursor = 2
            ExpectedSelectedPaths = None
        }
        {
            CursorMove = CursorToPath (createPath "/c/file4", false)
            ExpectedCursor = 1
            ExpectedSelectedPaths = None
        }
        {
            CursorMove = CursorToPath (createPath "/c/file4", true)
            ExpectedCursor = 2
            ExpectedSelectedPaths = None
        }
        {
            CursorMove = CursorToAndSelectPaths ([createPath "/c/file5"], false)
            ExpectedCursor = 2
            ExpectedSelectedPaths = Some []
        }
        {
            CursorMove = CursorToAndSelectPaths ([createPath "/c/file4"], false)
            ExpectedCursor = 1
            ExpectedSelectedPaths = Some []
        }
        {
            CursorMove = CursorToAndSelectPaths ([createPath "/c/file4"], true)
            ExpectedCursor = 2
            ExpectedSelectedPaths = Some []
        }
        {
            CursorMove = CursorToAndSelectPaths ([createPath "/c/file5"; createPath "/c/file4"], false)
            ExpectedCursor = 2
            ExpectedSelectedPaths = Some []
        }
        {
            CursorMove = CursorToAndSelectPaths ([createPath "/c/file5"; createPath "/c/file4"], true)
            ExpectedCursor = 2
            ExpectedSelectedPaths = Some ["/c/file5"; "/c/file4"]
        }
        {
            CursorMove = CursorToAndSelectPaths ([createPath "/c/file5"; createPath "/c/file3"], false)
            ExpectedCursor = 1
            ExpectedSelectedPaths = Some ["/c/file5"; "/c/file3"]
        }
    ] |> List.map TestCaseData

[<TestCaseSource(nameof cursorAndSelectionCases)>]
let ``listDirectory sets cursor and selection correctly`` case =
    let model =
        { testModel with
            Directory = cursorAndSelectionDirectory
            Cursor = 1
            SelectedItems = [cursorAndSelectionDirectory.[0]; cursorAndSelectionDirectory.[4]]
        }

    let actual = NavigationCommands.listDirectory case.CursorMove model

    let pathsToShow =
        match case.CursorMove with
        | CursorToPath (path, true) -> Set [path]
        | CursorToAndSelectPaths (paths, true) -> Set paths
        | _ -> Set.empty
    let expectedItems = cursorAndSelectionDirectory |> List.filter (fun i -> not i.IsHidden || pathsToShow |> Set.contains i.Path )
    let expectedSelectedPaths =
        case.ExpectedSelectedPaths
        |> Option.map (Seq.map createPath)
        |> Option.defaultWith (fun () -> model.SelectedItems |> Seq.map (fun i -> i.Path))
        |> Set
    let expected =
        { model with
            Items = expectedItems
            Cursor = case.ExpectedCursor
            SelectedItems = cursorAndSelectionDirectory |> List.filter (fun i -> expectedSelectedPaths |> Set.contains i.Path)
        }
    assertAreEqual expected actual
