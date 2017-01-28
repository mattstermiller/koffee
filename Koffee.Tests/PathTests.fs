module Koffee.PathTests

open NUnit.Framework
open FsUnit
open FsUnitTyped
open System.Reflection

let shouldParseTo expectedStr path =
    path |> shouldNotEqual None
    typedefof<Path>.GetProperty("Value", BindingFlags.NonPublic ||| BindingFlags.Instance)
        .GetValue(path.Value) |> should equal expectedStr

[<TestCase("")>]
[<TestCase("/")>]
[<TestCase("drives")>]
[<TestCase("DRives")>]
let ``Parse returns root for all valid values`` input =
    input |> Path.Parse |> shouldEqual (Some Path.Root)

[<TestCase("c:")>]
[<TestCase(@"c:\")>]
[<TestCase("c:/")>]
[<TestCase("/c")>]
[<TestCase("/c/")>]
[<TestCase(@"/c\")>]
let ``Parse returns drive for valid drives`` input =
    input |> Path.Parse |> shouldParseTo @"C:\"

[<TestCase(@"c:\test")>]
[<TestCase(@"c:\test\")>]
[<TestCase("c:/test")>]
[<TestCase("c:/test/")>]
[<TestCase("/c/test")>]
[<TestCase("/c/test/")>]
[<TestCase(@"/c\test")>]
[<TestCase(@"/c\test\")>]
let ``Parse returns path for valid paths`` input =
    input |> Path.Parse |> shouldParseTo @"C:\test"

[<TestCase("c")>]
[<TestCase("test")>]
[<TestCase("/test/")>]
[<TestCase("c:test")>]
[<TestCase(@"c\test\")>]
[<TestCase("/c:/test")>]
[<TestCase("/c/file?")>]
[<TestCase("/c/file*")>]
[<TestCase("/c/file<")>]
let ``Parse returns None for invalid paths`` input =
    input |> Path.Parse |> shouldEqual None
