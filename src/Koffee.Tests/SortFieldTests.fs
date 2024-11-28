module Koffee.SortFieldTests

open NUnit.Framework
open FsUnitTyped

let typeAndName item = (item.Type, item.Name)

let shouldEqualTypeAndName expected actual =
    shouldEqual (expected |> List.map typeAndName) (actual |> List.map typeAndName)

[<Test>]
let ``sortByTypeThen Name asc sorts folders then files by name`` () =
    SortField.sortByTypeThen Name false [
        createFolder "/c/dewberry"
        createFile "/c/cherry"
        createFolder "/c/banana"
        createFile "/c/apple"
    ]
    |> shouldEqualTypeAndName [
        createFolder "/c/banana"
        createFolder "/c/dewberry"
        createFile "/c/apple"
        createFile "/c/cherry"
    ]

[<Test>]
let ``sortByTypeThen Name desc sorts files then folders by name desc`` () =
    SortField.sortByTypeThen Name true [
        createFile "/c/apple"
        createFolder "/c/banana"
        createFile "/c/cherry"
        createFolder "/c/dewberry"
    ]
    |> shouldEqualTypeAndName [
        createFile "/c/cherry"
        createFile "/c/apple"
        createFolder "/c/dewberry"
        createFolder "/c/banana"
    ]

[<Test>]
let ``sortByTypeThen Name sorts insensitive to case`` () =
    SortField.sortByTypeThen Name false [
        createFile "/c/cherry"
        createFile "/c/Banana"
        createFile "/c/apple"
    ]
    |> shouldEqualTypeAndName [
        createFile "/c/apple"
        createFile "/c/Banana"
        createFile "/c/cherry"
    ]

[<Test>]
let ``sortByTypeThen Name sorts hyphenated names correctly`` () =
    SortField.sortByTypeThen Name false [
        createFile "/c/fileC"
        createFile "/c/fil-eA"
        createFile "/c/f-ileB"
    ]
    |> shouldEqualTypeAndName [
        createFile "/c/f-ileB"
        createFile "/c/fil-eA"
        createFile "/c/fileC"
    ]
