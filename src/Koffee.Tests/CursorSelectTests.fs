module Koffee.CursorSelectTests

open NUnit.Framework
open Acadian.FSharp
open Koffee

type TestCase = {
    Cursor: int
    PreviousSelect: (int * bool) option
    SelectedIndexes: int list
    ExpectedSelectedIndexes: int list
}

let testModel =
    let items = List.init 10 (sprintf "/c/file %i" >> createFile)
    { testModel with Directory = items; Items = items }

let test case =
    let itemsFromIndexes indexes =
        indexes |> List.map (testModel.Items.get_Item)

    let model =
        { testModel with
            Cursor = case.Cursor
            SelectedItems = itemsFromIndexes case.SelectedIndexes
            PreviousSelectIndexAndToggle = case.PreviousSelect
        }
    let actual = CursorCommands.selectRange model

    let expected =
        { model with
            SelectedItems = itemsFromIndexes case.ExpectedSelectedIndexes
            PreviousSelectIndexAndToggle = Some (model.Cursor, case.PreviousSelect |> Option.map snd |? true)
        }
    assertAreEqual expected actual

[<Test>]
let ``Select range with no previous selection selects cursor item`` () =
    test {
        Cursor = 1
        PreviousSelect = None
        SelectedIndexes = []
        ExpectedSelectedIndexes = [1]
    }

[<Test>]
let ``Select range with previous selection above cursor adds items from there to cursor`` () =
    test {
        Cursor = 7
        PreviousSelect = Some (4, true)
        SelectedIndexes = [1; 4]
        ExpectedSelectedIndexes = [1] @ [4..7]
    }

[<Test>]
let ``Select range with previous selection below cursor adds items from there to cursor`` () =
    test {
        Cursor = 1
        PreviousSelect = Some (4, true)
        SelectedIndexes = [7; 4]
        ExpectedSelectedIndexes = [7; 4] @ [1..3]
    }

[<Test>]
let ``Select range of already selected items only updates previous select index`` () =
    test {
        Cursor = 1
        PreviousSelect = Some (4, true)
        SelectedIndexes = [7] @ [1..4]
        ExpectedSelectedIndexes = [7] @ [1..4]
    }

[<Test>]
let ``Unselect range with previous unselection above cursor removes items from there to cursor`` () =
    test {
        Cursor = 7
        PreviousSelect = Some (4, false)
        SelectedIndexes = [3..8]
        ExpectedSelectedIndexes = [3; 8]
    }

[<Test>]
let ``Unselect range with previous unselection below cursor removes items from there to cursor`` () =
    test {
        Cursor = 1
        PreviousSelect = Some (4, false)
        SelectedIndexes = [7] @ [0..5]
        ExpectedSelectedIndexes = [7; 0; 5]
    }

[<Test>]
let ``Unselect range of already unselected items only updates previous select index`` () =
    test {
        Cursor = 1
        PreviousSelect = Some (4, false)
        SelectedIndexes = [0; 5]
        ExpectedSelectedIndexes = [0; 5]
    }
