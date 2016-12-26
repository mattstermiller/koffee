module Koffee.ValueConverterTests

open System.Windows.Data
open NUnit.Framework
open FsUnit
open Koffee.ValueConverters

type Toys =
    | Ball
    | Block
    | Book of string


let unionText = UnionText() :> IValueConverter

[<Test>]
let ``Convert() a union without a value returns the case name`` () =
    unionText.Convert(Ball, typedefof<string>, null, null)
    |> should equal "Ball"

[<Test>]
let ``Convert() a union with a value returns the case name`` () =
    unionText.Convert(Book "The Hobbit", typedefof<string>, null, null)
    |> should equal "Book"

[<Test>]
let ``ConvertBack() to union without a value gives union type`` () =
    unionText.ConvertBack("Ball", typedefof<Toys>, null, null)
    |> should equal Ball


let unionValue = UnionValue() :> IValueConverter

[<Test>]
let ``Convert() a union with a value returns the value`` () =
    unionValue.Convert(Book "The Hobbit", typedefof<string>, null, null)
    |> should equal "The Hobbit"

[<Test>]
let ``ConvertBack() to union with a value returns union type`` () =
    unionValue.ConvertBack("The Hobbit", typedefof<Toys>, null, null)
    |> should equal (Book "The Hobbit")


let optionValue = OptionValue() :> IValueConverter

[<Test>]
let ``Convert() an option with a value returns the value`` () =
    optionValue.Convert(Some 5, typedefof<string>, null, null)
    |> should equal 5

[<Test>]
let ``Convert() an option without a value returns null`` () =
    optionValue.Convert(None, typedefof<string>, null, null)
    |> should equal null

[<Test>]
let ``ConvertBack() to option with a value returns Some`` () =
    optionValue.ConvertBack(5, typedefof<int option>, null, null)
    |> should equal (Some 5)

[<Test>]
let ``ConvertBack() to option with null returns None`` () =
    optionValue.ConvertBack(null, typedefof<int option>, null, null)
    |> should equal None
