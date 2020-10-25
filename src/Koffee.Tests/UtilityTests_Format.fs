module Koffee.UtilityTests_Format

open NUnit.Framework
open FsUnitTyped

type FakeEnvVarProvider() =
    interface Format.ISubstitutionProvider with
        member _.getSubstitution key =
            match key with
            | "var" -> Some "REPLACED"
            | _ -> None

let fakeEnvVarProvider = new FakeEnvVarProvider()
let subFakeVars = Format.subVars fakeEnvVarProvider

[<TestCase("Some %var% value", "Some REPLACED value")>]
[<TestCase("Some %var%var% value", "Some REPLACEDvar% value")>]
[<TestCase("Some %var%% value", "Some REPLACED% value")>]
[<TestCase("Two %var%%var% values", "Two REPLACEDREPLACED values")>]
[<TestCase("Some %var%", "Some REPLACED")>]
[<TestCase("%var% value", "REPLACED value")>]
[<TestCase("%var%", "REPLACED")>]
let ``Replaces %var% in string`` input expected =
    // Act
    let result = subFakeVars input

    // Assert
    result |> shouldEqual expected

[<TestCase("Some %var value")>]
[<TestCase("Some var% value")>]
[<TestCase("Some value")>]
let ``Leaves non-%var%'s alone`` input =
    // Act
    let result = subFakeVars input

    // Assert
    result |> shouldEqual input

[<TestCase("Some %unknown% value")>]
[<TestCase("Some %var2% value")>]
[<TestCase("Some %%var% value")>]
let ``Leaves unknown vars alone`` input =
    // Act
    let result = subFakeVars input

    // Assert
    result |> shouldEqual input

[<Test>]
let ``Replaces %envvar% in string`` () =
    // Arrange
    let input = "Some %envvar% value"
    let expected = "Some REPLACED value"
    System.Environment.SetEnvironmentVariable("envvar", "REPLACED")

    // Act
    let result = Format.subEnvVars input

    // Assert
    result |> shouldEqual expected
