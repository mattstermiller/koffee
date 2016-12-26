module Koffee.MainControllerTests_BackForward

open System.Windows.Input
open FSharp.Desktop.UI
open NUnit.Framework
open FsUnitTyped
open Foq

let createController () =
    let fileSys =
        Mock<IFileSystemService>()
            .Setup(fun x -> <@ x.Normalize(any()) @>).Calls<Path>(fun p -> p)
            .Setup(fun x -> <@ x.GetNodes(any()) @>).Returns([])
            .Create()
    let settingsFactory () = Mock.Of<Mvc<SettingsEvents, SettingsModel>>()
    MainController(fileSys, settingsFactory)

let history contrFunc pathStr cursor backStack forwardStack =
    let model = MainModel.Create<MainModel>()
    model.BackStack <- backStack |> List.map (fun (p, c) -> (Path p, c))
    model.ForwardStack <- forwardStack |> List.map (fun (p, c) -> (Path p, c))
    model.Path <- Path pathStr
    model.Cursor <- cursor
    model.Status <- "previous status"
    let contr = createController()
    contrFunc contr model
    model

let back = history (fun contr -> contr.Back)
let forward = history (fun contr -> contr.Forward)

[<Test>]
let ``Back without history does nothing``() =
    let model = back "c:" 1 [] []
    model.Path |> shouldEqual (Path "c:")
    model.Cursor |> shouldEqual 1
    model.BackStack |> shouldEqual []
    model.ForwardStack |> shouldEqual []
    model.Status |> shouldEqual "previous status"

[<Test>]
let ``Back with simple history changes path and stacks``() =
    let model = back "orig" 1 ["back", 2] []
    model.Path |> shouldEqual (Path "back")
    model.Cursor |> shouldEqual 2
    model.BackStack |> shouldEqual []
    model.ForwardStack |> shouldEqual [Path "orig", 1]
    model.Status |> shouldEqual ""

[<Test>]
let ``Back with more history changes path and stacks``() =
    let model = back "orig" 1 ["back1", 2; "back2", 3] ["fwd1", 4; "fwd2", 5]
    model.Path |> shouldEqual (Path "back1")
    model.Cursor |> shouldEqual 2
    model.BackStack |> shouldEqual [Path "back2", 3]
    model.ForwardStack |> shouldEqual [Path "orig", 1; Path "fwd1", 4; Path "fwd2", 5]
    model.Status |> shouldEqual ""

[<Test>]
let ``Forward without history does nothing``() =
    let model = forward "c:" 1 [] []
    model.Path |> shouldEqual (Path "c:")
    model.Cursor |> shouldEqual 1
    model.BackStack |> shouldEqual []
    model.ForwardStack |> shouldEqual []
    model.Status |> shouldEqual "previous status"

[<Test>]
let ``Forward with simple history changes path and stacks``() =
    let model = forward "orig" 1 [] ["fwd", 2]
    model.Path |> shouldEqual (Path "fwd")
    model.Cursor |> shouldEqual 2
    model.BackStack |> shouldEqual [Path "orig", 1]
    model.ForwardStack |> shouldEqual []
    model.Status |> shouldEqual ""

[<Test>]
let ``Forward with more history changes path and stacks``() =
    let model = forward "orig" 1 ["back1", 2; "back2", 3] ["fwd1", 4; "fwd2", 5]
    model.Path |> shouldEqual (Path "fwd1")
    model.Cursor |> shouldEqual 4
    model.BackStack |> shouldEqual [Path "orig", 1; Path "back1", 2; Path "back2", 3]
    model.ForwardStack |> shouldEqual [Path "fwd2", 5]
    model.Status |> shouldEqual ""
