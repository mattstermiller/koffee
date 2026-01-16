module Koffee.MainModelTests

open NUnit.Framework
open FsUnitTyped
open Acadian.FSharp

[<TestCase(5, 1, 51)>]
[<TestCase(5, 0, 50)>]
let ``Append repeat digit on existing appends`` current append expected =
    let model = { MainModel.Default with RepeatCommand = Some current }
    let result = model |> MainModel.appendRepeatDigit append
    result.RepeatCommand |> shouldEqual (Some expected)

[<Test>]
let ``Append positive repeat digit to none sets it`` () =
    let model = { MainModel.Default with RepeatCommand = None }
    let result = model |> MainModel.appendRepeatDigit 1
    result.RepeatCommand |> shouldEqual (Some 1)

[<Test>]
let ``Append zero repeat digit to none does nothing`` () =
    let model = { MainModel.Default with RepeatCommand = None }
    let result = model |> MainModel.appendRepeatDigit 0
    result.RepeatCommand |> shouldEqual None


[<Test>]
let ``MainBindings.Default does not contain conflicting mappings`` () =
    MainBindings.Default
    |> List.groupBy (fun kb -> kb.KeyCombo)
    |> List.filter (snd >> List.tail >> Seq.isNotEmpty)
    |> shouldEqual []
