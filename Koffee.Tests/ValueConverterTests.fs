namespace Koffee.Tests.ValueConverterTests

open System.Windows.Data
open NUnit.Framework
open FsUnit
open Koffee.ValueConverters

type Toys =
    | Ball
    | Block
    | Book of string

[<TestFixture>]
type ``UnionText tests``() =
    let conv = UnionText() :> IValueConverter

    [<Test>] member x.``Convert() a union without a value returns the case name`` () =
        conv.Convert(Ball, typedefof<string>, null, null)
        |> should equal "Ball"

    [<Test>] member x.``Convert() a union with a value returns the case name`` () =
        conv.Convert(Book "The Hobbit", typedefof<string>, null, null)
        |> should equal "Book"

    [<Test>] member x.``ConvertBack() to union without a value gives union type`` () =
        conv.ConvertBack("Ball", typedefof<Toys>, null, null)
        |> should equal Ball

[<TestFixture>]
type ``UnionValue tests``() =
    let conv = UnionValue() :> IValueConverter

    [<Test>] member x.``Convert() a union with a value returns the value`` () =
        conv.Convert(Book "The Hobbit", typedefof<string>, null, null)
        |> should equal "The Hobbit"

    [<Test>] member x.``ConvertBack() to union with a value returns union type`` () =
        conv.ConvertBack("The Hobbit", typedefof<Toys>, null, null)
        |> should equal (Book "The Hobbit")

[<TestFixture>]
type ``OptionValue tests``() =
    let conv = OptionValue() :> IValueConverter

    [<Test>] member x.``Convert() an option with a value returns the value`` () =
        conv.Convert(Some 5, typedefof<string>, null, null)
        |> should equal 5

    [<Test>] member x.``Convert() an option without a value returns null`` () =
        conv.Convert(None, typedefof<string>, null, null)
        |> should equal null

    [<Test>] member x.``ConvertBack() to option with a value returns Some`` () =
        conv.ConvertBack(5, typedefof<int option>, null, null)
        |> should equal (Some 5)

    [<Test>] member x.``ConvertBack() to option with null returns None`` () =
        conv.ConvertBack(null, typedefof<int option>, null, null)
        |> should equal None
