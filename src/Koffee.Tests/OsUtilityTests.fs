module Koffee.OsUtilityTests

open NUnit.Framework
open FsUnitTyped

type FakeEnvVarsOperatingSystem() =
    inherit OperatingSystem() with
    interface IOperatingSystem with
        override _.GetEnvironmentVariable key =
            match key with
            | "var" -> Some "REPLACED"
            | _ -> None

let fakeEnvVarsOperatingSystem = new FakeEnvVarsOperatingSystem()
let subFakeVars = OsUtility.subEnvVars fakeEnvVarsOperatingSystem

[<TestCase("Some %var% value", "Some REPLACED value")>]
[<TestCase("Some %var%var% value", "Some REPLACEDvar% value")>]
[<TestCase("Some %var%% value", "Some REPLACED% value")>]
[<TestCase("Two %var%%var% values", "Two REPLACEDREPLACED values")>]
[<TestCase("Some %var%", "Some REPLACED")>]
[<TestCase("%var% value", "REPLACED value")>]
[<TestCase("%var%", "REPLACED")>]
let ``Replaces var in string`` input expected =
    let result = subFakeVars input
    result |> shouldEqual expected

[<TestCase("Some %var value")>]
[<TestCase("Some var% value")>]
[<TestCase("Some value")>]
let ``Leaves non vars alone`` input =
    let result = subFakeVars input
    result |> shouldEqual input

[<TestCase("Some %unknown% value")>]
[<TestCase("Some %var2% value")>]
[<TestCase("Some %%var% value")>]
let ``Leaves unknown vars alone`` input =
    let result = subFakeVars input
    result |> shouldEqual input

[<Test>]
let ``Replaces envvar in string`` () =
    let input = "Some %envvar% value"
    let expected = "Some REPLACED value"
    let os = new OperatingSystem()
    System.Environment.SetEnvironmentVariable("envvar", "REPLACED")

    let result = OsUtility.subEnvVars os input

    result |> shouldEqual expected
