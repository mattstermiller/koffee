module Koffee.MainModelTests

open NUnit.Framework
open FsUnitTyped

[<TestCase(5, 1, 51)>]
[<TestCase(5, 0, 50)>]
let ``Append count on existing`` current append expected =
    let model = { MainModel.Default with RepeatCommand = Some current }
    let result = model |> MainModel.appendRepeatDigit append
    result.RepeatCommand |> shouldEqual (Some expected)

[<Test>]
let ``Append count when none set sets it`` () =
    let model = { MainModel.Default with RepeatCommand = None }
    let result = model |> MainModel.appendRepeatDigit 1
    result.RepeatCommand |> shouldEqual (Some 1)

[<Test>]
let ``Append 0 count when none set does nothing`` () =
    let model = { MainModel.Default with RepeatCommand = None }
    let result = model |> MainModel.appendRepeatDigit 0
    result.RepeatCommand |> shouldEqual None
