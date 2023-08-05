module Koffee.PathTests

open NUnit.Framework
open FsUnitTyped
open System.Reflection
open System

let getPathValue (path: Path) =
    typedefof<Path>.GetProperty("Value", BindingFlags.NonPublic ||| BindingFlags.Instance)
                   .GetValue(path) :?> string

let shouldParseTo expectedStr path =
    path |> Option.map getPathValue
         |> shouldEqual (Some expectedStr)

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
let ``Parse returns path for valid local paths`` input =
    input |> Path.Parse |> shouldParseTo @"C:\test"

[<TestCase(@"network")>]
[<TestCase(@"NETwork")>]
[<TestCase("/net")>]
[<TestCase("/NET/")>]
let ``Parse returns Network for all valid values`` input =
    input |> Path.Parse |> shouldParseTo "Network"

[<TestCase(@"\\server", @"\\server")>]
[<TestCase(@"\\server\", @"\\server")>]
[<TestCase(@"/net/server", @"\\server")>]
[<TestCase(@"/net/server/", @"\\server")>]
[<TestCase(@"\\Serv_01\", @"\\Serv_01")>]
[<TestCase(@"/net/Serv_01/", @"\\Serv_01")>]
[<TestCase(@"\\Serv-R\", @"\\Serv-R")>]
[<TestCase(@"/net/Serv-R/", @"\\Serv-R")>]
[<TestCase(@"\\127.0.0.1\", @"\\127.0.0.1")>]
[<TestCase(@"/net/127.0.0.1/", @"\\127.0.0.1")>]
let ``Parse returns path for valid server path`` input expectedServerName =
    input |> Path.Parse |> shouldParseTo expectedServerName

[<TestCase(@"\\server\share")>]
[<TestCase(@"\\server\share\")>]
[<TestCase(@"//server/share")>]
[<TestCase(@"/net/server/share")>]
[<TestCase(@"/net/server/share/")>]
let ``Parse returns path for valid network paths`` input =
    input |> Path.Parse |> shouldParseTo @"\\server\share"

[<TestCase("~", "")>]
[<TestCase("~/", "")>]
[<TestCase("~/test", @"\test")>]
[<TestCase(@"~\", "")>]
[<TestCase(@"~\test", @"\test")>]
let ``Parse substitutes tilde for user directory`` input expectedSuffix =
    let expectedPath = Path.UserDirectory + expectedSuffix
    input |> Path.Parse |> shouldParseTo expectedPath

[<TestCase("c")>]
[<TestCase("test")>]
[<TestCase("/test/")>]
[<TestCase("c:test")>]
[<TestCase(@"c\test\")>]
[<TestCase("/c:/test")>]
[<TestCase("/c/file?")>]
[<TestCase("/c/file*")>]
[<TestCase("/c/file<")>]
[<TestCase(@"\\")>]
[<TestCase("//")>]
[<TestCase(@"\\serv er")>]
[<TestCase("/net/serv er")>]
let ``Parse returns None for invalid paths`` input =
    input |> Path.Parse |> shouldEqual None


[<TestCase("/c", "C:")>]
[<TestCase("/c/file", "file")>]
[<TestCase("/net", "Network")>]
let ``Name returns expected value`` pathStr expected =
    (createPath pathStr).Name |> shouldEqual expected

[<TestCase("", "")>]
[<TestCase(@"C:\", "")>]
[<TestCase(@"C:\test", @"C:\")>]
[<TestCase(@"C:\test\a folder", @"C:\test")>]
[<TestCase("Network", "")>]
[<TestCase(@"\\server", "Network")>]
[<TestCase(@"\\server\share", @"\\server")>]
[<TestCase(@"\\server\share\a folder", @"\\server\share")>]
let ``Parent returns expected value`` pathStr expected =
    (createPath pathStr).Parent |> getPathValue |> shouldEqual expected

[<TestCase(@"Drives", @"Drives")>]
[<TestCase(@"Network", @"Network")>]
[<TestCase(@"c:\", @"c:\")>]
[<TestCase(@"c:\things", @"c:\")>]
[<TestCase(@"c:\things\stuff", @"c:\")>]
[<TestCase(@"\\server", @"\\server")>]
[<TestCase(@"\\server\share", @"\\server\share")>]
[<TestCase(@"\\server\share\things", @"\\server\share")>]
[<TestCase(@"\\server\share\things\stuff", @"\\server\share")>]
let ``Base returns expected path`` path expected =
    let actual = (createPath path).Base
    let expected = createPath expected
    actual |> shouldEqual expected

[<TestCase(@"", "/")>]
[<TestCase(@"C:\", "/c/")>]
[<TestCase(@"C:\test", "/c/test")>]
[<TestCase(@"C:\test\a folder", "/c/test/a folder")>]
[<TestCase(@"Network", "/net/")>]
[<TestCase(@"\\server", "/net/server")>]
[<TestCase(@"\\server\share", "/net/server/share")>]
let ``Format drive in Unix`` pathStr expected =
    (createPath pathStr).Format Unix |> shouldEqual expected

[<Test>]
let ``TypeConverter to string`` () =
    let conv = System.ComponentModel.TypeDescriptor.GetConverter typeof<Path>
    let pathString = @"C:\Sample"
    createPath pathString
    |> conv.ConvertToString
    |> shouldEqual pathString

[<Test>]
let ``TypeConverter from string`` () =
    let conv = System.ComponentModel.TypeDescriptor.GetConverter typeof<Path>
    let pathString = @"C:\Sample"
    let expected = createPath pathString
    conv.ConvertFromString pathString
    :?> Path
    |> shouldEqual expected

[<TestCase(@"C:\Sample", @"C:\Sample", 0)>]
[<TestCase(@"C:\SAMPLE", @"C:\sample", 0)>]
[<TestCase(@"C:\Sample2", @"C:\Sample1", 1)>]
let ``Compare Windows paths`` aPath bPath expected =
    let aComp = createPath aPath :> IComparable
    let bComp = createPath bPath :> IComparable
    aComp.CompareTo bComp |> shouldEqual expected
    bComp.CompareTo aComp |> shouldEqual -expected

[<TestCase("/")>]
[<TestCase("/c")>]
[<TestCase("/c/folder")>]
[<TestCase("/c/folder/sub")>]
[<TestCase("/c/folder/sub/file")>]
let ``IsWithin returns true if contains path`` path =
    (createPath "/c/folder/sub/file").IsWithin (createPath path)
    |> shouldEqual true

[<TestCase("/d")>]
[<TestCase("/c/folde")>]
[<TestCase("/c/folderr")>]
[<TestCase("/c/folder/sub/fil")>]
let ``IsWithin returns false if does not contain path`` path =
    (createPath "/c/folder/sub/file").IsWithin (createPath path)
    |> shouldEqual false

[<TestCase("/c", "/c/new/folder/sub/file")>]
[<TestCase("/c/folder", "/c/new/sub/file")>]
[<TestCase("/c/folder/sub", "/c/new/file")>]
[<TestCase("/c/folder/sub/file", "/c/new")>]
let ``TryReplace returns new path if contains path`` path expectedPath =
    (createPath "/c/folder/sub/file").TryReplace (createPath path) (createPath "/c/new")
    |> shouldEqual (Some (createPath expectedPath))

[<TestCase("/")>]
[<TestCase("/d")>]
[<TestCase("/c/folde")>]
[<TestCase("/c/folderr")>]
[<TestCase("/c/folder/sub/fil")>]
let ``TryReplace returns None if does not contain path`` path =
    (createPath "/c/folder/sub/file").TryReplace (createPath path) (createPath "/c/new")
    |> shouldEqual None
