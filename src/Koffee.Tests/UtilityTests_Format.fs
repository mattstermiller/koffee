module Koffee.UtilityTests_Format

open NUnit.Framework
open FsUnitTyped

type FakeSubstitutionProvider() =
    interface Format.ISubstitutionProvider with
        member _.getSubstitution key =
            match key with
            | "var" -> Some "REPLACED"
            | _ -> None

let fakeEnvVarProvider = new FakeSubstitutionProvider()
let subFakeVars = Format.subVars fakeEnvVarProvider

[<Test>]
let ``Replaces %var% in string`` () =
    // Arrange
    let input = "Some %var% value"
    let expected = "Some REPLACED value"

    // Act
    let result = subFakeVars input

    // Assert
    result |> shouldEqual expected

[<TestCase("Some %var value")>]
[<TestCase("Some var% value")>]
let ``Leaves unfinished alone`` input =
    // Act
    let result = subFakeVars input

    // Assert
    result |> shouldEqual input

[<TestCase("Some %unknown% value")>]
[<TestCase("Some %var2% value")>]
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
