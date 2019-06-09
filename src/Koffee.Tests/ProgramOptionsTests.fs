module Koffee.ProgramOptionsTests

open NUnit.Framework
open FsUnitTyped
open ProgramOptions

[<TestCase(@"c:\test")>]
[<TestCase(@"--arg=1|c:\test")>]
[<TestCase(@"--arg=1|c:\test|--do=something")>]
let ``parseArgs parses path correctly`` (args: string) =
    let args = args.Split('|') |> Array.toList
    parseArgs args |> shouldEqual { StartPath = Some @"c:\test"; StartLocation = None; StartSize = None }

[<TestCase(@"--location=1,2|--size=3,4")>]
[<TestCase(@"--Size=3,4|--lOcaTion=1,2|--size=9,9")>]
let ``parseArgs parses location and size correctly`` (args: string) =
    let args = args.Split('|') |> Array.toList
    parseArgs args |> shouldEqual { StartPath = None; StartLocation = Some (1, 2); StartSize = Some (3, 4) }

[<TestCase(@"-size=9,9")>]
[<TestCase(@"--size=9,a")>]
[<TestCase(@"--size=a,9")>]
[<TestCase(@"-location=9,9")>]
[<TestCase(@"--location=9,a")>]
[<TestCase(@"--location=a,9")>]
let ``parseArgs handles invalid input`` (args: string) =
    let args = args.Split('|') |> Array.toList
    parseArgs args |> shouldEqual { StartPath = None; StartLocation = None; StartSize = None }
