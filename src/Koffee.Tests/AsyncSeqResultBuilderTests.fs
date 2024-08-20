module Koffee.AsyncSeqResultBuilderTests

open NUnit.Framework
open FsUnitTyped
open FSharp.Control

let shouldEqualList expected actual =
    actual |> AsyncSeq.toListSynchronously |> shouldEqual expected

[<Test>]
let ``Yield works as expected`` () =
    asyncSeqResult {
        yield 1
        yield 2
        yield 3
    }
    |> shouldEqualList [
        Ok 1
        Ok 2
        Ok 3
    ]

[<Test>]
let ``YieldFrom Result does not call continuation after error`` () =
    asyncSeqResult {
        yield 1
        yield! Error "e"
        yield 2
        failwith "unreachable!"
    } |> shouldEqualList [
        Ok 1
        Error "e"
    ]

[<Test>]
let ``YieldFrom AsyncSeq of Result yields up to and including error and does not call continuation after error`` () =
    asyncSeqResult {
        yield 1
        yield! asyncSeq {
            yield Ok 2
            yield Error "e"
            yield Ok 3
        }
        yield 4
        failwith "unreachable!"
    } |> shouldEqualList [
        Ok 1
        Ok 2
        Error "e"
    ]

[<Test>]
let ``Return does not call continuation after error`` () =
    asyncSeqResult {
        yield 1
        return "e"
        yield 2
        failwith "unreachable!"
    } |> shouldEqualList [
        Ok 1
        Error "e"
    ]

[<Test>]
let ``Bind Result continues on Ok but not on Error`` () =
    asyncSeqResult {
        let! number = Ok 1
        yield number
        let! _ = Error "e"
        failwith "unreachable!"
    } |> shouldEqualList [
        Ok 1
        Error "e"
    ]

[<Test>]
let ``Bind AsyncSeq of Result yields results up to and including Error and does not continue after Error`` () =
    asyncSeqResult {
        let! latestNumber = asyncSeq {
            Ok 1
            Ok 2
        }
        yield latestNumber * 10
        let! _ = asyncSeq {
            Ok 3
            Error "e"
            Ok 4
        }
        failwith "unreachable!"
    } |> shouldEqualList [
        Ok 1
        Ok 2
        Ok 20
        Ok 3
        Error "e"
    ]

[<Test>]
let ``For seq of values iterates until error is yielded`` () =
    asyncSeqResult {
        let vals = [1..3]
        for v in vals do
            if v = 3 then
                return "e"
                failwith "unreachable!"
            yield v * 10
        failwith "unreachable!"
    } |> shouldEqualList [
        Ok 10
        Ok 20
        Error "e"
    ]

[<Test>]
let ``For AsyncSeq of values iterates until error is yielded`` () =
    asyncSeqResult {
        let vals = AsyncSeq.ofSeq [1..3]
        for v in vals do
            if v = 3 then
                return "e"
                failwith "unreachable!"
            yield v * 10
        failwith "unreachable!"
    } |> shouldEqualList [
        Ok 10
        Ok 20
        Error "e"
    ]
