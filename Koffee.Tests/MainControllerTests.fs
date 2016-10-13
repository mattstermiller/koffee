namespace Koffee.Tests

open System.Windows.Input
open FSharp.Desktop.UI
open NUnit.Framework
open FsUnit
open Foq
open Koffee

[<TestFixture>]
type MainControllerTests() =
    let nodes names =
        let toNode name = {
            Name = name
            Path = Path ""
            Type = Folder
            Modified = None
            Size = None
        }
        names |> Seq.map toNode |> Seq.toList

    let find char cursorStart =
        let model = MainModel.Create<MainModel>()
        model.Nodes <- "alice,bob,charlie,crystal,apple,cherry".Split(',') |> nodes
        model.Cursor <- cursorStart

        let pathing = Mock.Of<IPathService>()
        let contr = MainController(pathing)
        contr.Find char model
        model.Cursor

    [<Test>]
    member x.``Find a char that matches the next node should set the cursor to the next index``() =
        find 'c' 2 |> should equal 3

    [<Test>]
    member x.``Find a char that matches only the current node should not change the cursor``() =
        find 'b' 1 |> should equal 1

    [<Test>]
    member x.``Find a char that matches a node wrapping around should set the cursor to the that index``() =
        find 'b' 2 |> should equal 1
