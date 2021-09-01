module Koffee.ProgramOptionsTests

open NUnit.Framework
open FsUnitTyped
open ProgramOptions
open Acadian.FSharp

[<TestCase(@"c:\test", @"c:\test")>]
[<TestCase(@"c:\test", @"--arg=1 c:\test")>]
[<TestCase(@"c:\test", @"--arg=1 c:\test --do=something")>]
[<TestCase(@"C:\", @"c:")>]
[<TestCase(@"Drives", @"drives")>]
[<TestCase(@"Network", @"network")>]
[<TestCase(@"\\server", @"\\server")>]
[<TestCase(@"\\server\share", @"\\server\share")>]
let ``parseArgs parses path correctly`` expected args =
    let args = args |> String.split ' ' |> Array.toList
    parseArgs args |> shouldEqual { StartPath = Some expected; StartLocation = None; StartSize = None }

[<TestCase(@"--location=1,2 --size=3,4")>]
[<TestCase(@"--Size=3,4 --lOcaTion=1,2 --size=9,9")>]
let ``parseArgs parses location and size correctly`` args =
    let args = args |> String.split ' ' |> Array.toList
    parseArgs args |> shouldEqual { StartPath = None; StartLocation = Some (1, 2); StartSize = Some (3, 4) }

[<TestCase(@"-size=9,9")>]
[<TestCase(@"--size=9,a")>]
[<TestCase(@"--size=a,9")>]
[<TestCase(@"-location=9,9")>]
[<TestCase(@"--location=9,a")>]
[<TestCase(@"--location=a,9")>]
let ``parseArgs handles invalid input`` args =
    let args = args |> String.split ' ' |> Array.toList
    parseArgs args |> shouldEqual { StartPath = None; StartLocation = None; StartSize = None }
