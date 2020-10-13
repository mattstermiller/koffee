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


let parseForTest pathStr =
    match Path.Parse pathStr with
    | Some p -> p
    | None -> failwithf "Test path string '%s' does not parse" pathStr

[<TestCase("", "")>]
[<TestCase(@"C:\", "")>]
[<TestCase(@"C:\test", @"C:\")>]
[<TestCase(@"C:\test\a folder", @"C:\test")>]
[<TestCase("Network", "")>]
[<TestCase(@"\\server", "Network")>]
[<TestCase(@"\\server\share", @"\\server")>]
[<TestCase(@"\\server\share\a folder", @"\\server\share")>]
let ``Parent returns expected value`` pathStr expected =
    (parseForTest pathStr).Parent |> getPathValue |> shouldEqual expected

[<TestCase(@"", "/")>]
[<TestCase(@"C:\", "/c/")>]
[<TestCase(@"C:\test", "/c/test")>]
[<TestCase(@"C:\test\a folder", "/c/test/a folder")>]
[<TestCase(@"Network", "/net/")>]
[<TestCase(@"\\server", "/net/server")>]
[<TestCase(@"\\server\share", "/net/server/share")>]
let ``Format drive in Unix`` pathStr expected =
    (parseForTest pathStr).Format Unix |> shouldEqual expected

[<Test>]
let ``TypeConverter to string`` () =
    let conv = System.ComponentModel.TypeDescriptor.GetConverter typeof<Path>
    let pathString = @"C:\Sample"
    parseForTest pathString
    |> conv.ConvertToString
    |> shouldEqual pathString
    
[<Test>]
let ``TypeConverter from string`` () =
    let conv = System.ComponentModel.TypeDescriptor.GetConverter typeof<Path>
    let pathString = @"C:\Sample"
    let expected = parseForTest pathString
    conv.ConvertFromString pathString
    :?> Path
    |> shouldEqual expected

let testComparability aPath bPath expected =
    let aComp = parseForTest aPath :> IComparable
    let bComp = parseForTest bPath :> IComparable
    aComp.CompareTo bComp |> shouldEqual expected
    bComp.CompareTo aComp |> shouldEqual -expected

[<TestCase(@"C:\Sample", @"C:\Sample", 0)>]
[<TestCase(@"C:\Sample1", @"C:\Sample2", -1)>]
[<TestCase(@"C:\Sample2", @"C:\Sample1", 1)>]
let ``Compare Windows paths`` aPath bPath expected =
    testComparability aPath bPath expected

[<TestCase(@"/c/Sample", @"/c/Sample", 0)>]
[<TestCase(@"/c/Sample1", @"/c/Sample2", -1)>]
[<TestCase(@"/c/Sample2", @"/c/Sample1", 1)>]
let ``Compare Unix paths`` aPath bPath expected =
    testComparability aPath bPath expected

[<TestCase(@"C:/Sample", @"/c/Sample", 0)>]
[<TestCase(@"C:/Sample1", @"/c/Sample2", -1)>]
[<TestCase(@"C:/Sample2", @"/c/Sample1", 1)>]
let ``Compare Windows and Unix paths`` aPath bPath expected =
    testComparability aPath bPath expected

