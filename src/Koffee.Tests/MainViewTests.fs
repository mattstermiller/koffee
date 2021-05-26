module Koffee.MainViewTests

open NUnit.Framework
open FsUnitTyped

[<TestCase(1, 1, 1, 1)>]
[<TestCase(3, 3, 3, 3)>]
[<TestCase(4, 4, 3, 3)>]
[<TestCase(4, 2, 4, 2)>]
[<TestCase(6, 0, 6, 0)>]
[<TestCase(7, 0, 6, 0)>]
[<TestCase(6, 1, 5, 1)>]
[<TestCase(0, 7, 0, 6)>]
[<TestCase(1, 6, 1, 5)>]
let trimStacks_limits_combined_length_to_double_stackSize prevLen nextLen expPrevLen expNextLen =
    let prev = List.init prevLen ((+) 1)
    let next = List.init nextLen ((+) 1)
    let expPrev = prev |> List.truncate expPrevLen
    let expNext = next |> List.truncate expNextLen
    let (actualPrev, actualNext) = MainView.trimStacks 3 prev next
    (actualPrev, actualNext) |> shouldEqual (expPrev, expNext)
