module Koffee.UtilityTests_Math

open NUnit.Framework
open FsUnitTyped

let allDigitChars =
    Array.init 10 (fun i -> [| box (string i).[0]; box i |])

[<TestCaseSource("allDigitChars")>]
let ``parseDigit with valid digit`` char expected =
    Math.parseDigit char
    |> assertOk
    |> shouldEqual expected

[<Test>]
let ``parseDigit errors on invalid digit`` () =
    Math.parseDigit 'm'
    |> assertError |> ignore

[<TestCase('1', 1, 11)>]
[<TestCase('7', 133, 1337)>]
[<TestCase('0', 32, 320)>]
let ``appendDigit with positive number`` char number expected =
    Math.appendDigit char number
    |> assertOk |> shouldEqual expected

[<TestCase('1', -1, -11)>]
[<TestCase('7', -133, -1337)>]
[<TestCase('0', -32, -320)>]
let ``appendDigit with negative number`` char number expected =
    Math.appendDigit char number
    |> assertOk |> shouldEqual expected

[<TestCase('1', 0, 1)>]
[<TestCase('7', 0, 7)>]
[<TestCase('0', 0, 0)>]
let ``appendDigit errors when append to zero`` char number expected =
    Math.appendDigit char number
    |> assertError |> ignore

[<Test>]
let ``appendDigit errors on invalid digit`` () =
    Math.appendDigit 'm' 42
    |> assertError |> ignore

