module Koffee.MainLogicTests_CursorMove

open NUnit.Framework
open FsUnitTyped
open Koffee.MainLogic

let modelWithItems =
    { MainModel.Default with
        Items = [
            for i in 0..100 -> createFile (sprintf "/c/Item%i" i)
        ]
    }

[<TestCase(42, 1, 43)>]
[<TestCase(42, -1, 41)>]
[<TestCase(42, 0, 42)>]
let ``Zero count -> moves normally`` pos delta expected =
    // Arrange
    let model =
        { modelWithItems with
            Cursor = pos
            KeyComboCount = 0
        }

    // Act
    let result = Nav.MoveWithKeyComboCount delta model

    // Assert
    result.Cursor |> shouldEqual expected

[<TestCase(42, 1, 49)>]
[<TestCase(42, -1, 35)>]
[<TestCase(42, 0, 42)>]
let ``7 count -> moves x7`` pos delta expected =
    // Arrange
    let model =
        { modelWithItems with
            Cursor = pos
            KeyComboCount = 7
        }

    // Act
    let result = Nav.MoveWithKeyComboCount delta model

    // Assert
    result.Cursor |> shouldEqual expected
