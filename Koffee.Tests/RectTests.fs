module Koffee.RectTests

open NUnit.Framework
open FsUnitTyped
open Utility

let container = Rect.ofPairs (2, 1) (20, 10)

let fit loc size =
    Rect.ofPairs loc size |> Rect.fit container

[<TestCase(2, 1, 1, 1)>]
[<TestCase(2, 1, 20, 10)>]
[<TestCase(21, 10, 1, 1)>]
[<TestCase(8, 4, 10, 5)>]
let ``fit does not change anything when rectangle is within container`` left top width height =
    fit (left, top) (width, height) |> shouldEqual (Rect.ofPairs (left, top) (width, height))

[<TestCase(0, 0, 1, 1, 2, 1)>]
[<TestCase(0, 0, 20, 10, 2, 1)>]
[<TestCase(22, 11, 2, 1, 20, 10)>]
[<TestCase(22, 11, 10, 5, 12, 6)>]
[<TestCase(22, 11, 20, 10, 2, 1)>]
let ``fit changes location when size is within half`` left top width height expectedLeft expectedTop =
    fit (left, top) (width, height) |> shouldEqual (Rect.ofPairs (expectedLeft, expectedTop) (width, height))

[<TestCase(2, 1, 21, 11)>]
[<TestCase(0, 0, 21, 11)>]
[<TestCase(99, 99, 99, 99)>]
let ``fit changes location and size when size is over half`` left top width height =
    fit (left, top) (width, height) |> shouldEqual container
