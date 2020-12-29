module Koffee.MainModelTests

open NUnit.Framework
open FsUnitTyped

[<TestCase(5, 1, 51)>]
[<TestCase(5, 0, 50)>]
let ``Append count on existing`` current append expected =
    // Arrange
    let model = { MainModel.Default with RepeatCommand = Some current }

    // Act
    let result = model.AppendRepeatDigit append

    // Assert
    result.RepeatCommand |> shouldEqual (Some expected)

[<Test>]
let ``Append count when none set sets it`` () =
    // Arrange
    let model = { MainModel.Default with RepeatCommand = None }

    // Act
    let result = model.AppendRepeatDigit 1

    // Assert
    result.RepeatCommand |> shouldEqual (Some 1)

[<Test>]
let ``Append 0 count when none set does nothing`` () =
    // Arrange
    let model = { MainModel.Default with RepeatCommand = None }

    // Act
    let result = model.AppendRepeatDigit 0

    // Assert
    result.RepeatCommand |> shouldEqual None
