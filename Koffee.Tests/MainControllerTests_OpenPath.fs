module Koffee.MainControllerTests_OpenPath

open System
open System.Windows.Input
open FSharp.Desktop.UI
open NUnit.Framework
open FsUnit
open FsUnitTyped
open Foq
open KellermanSoftware.CompareNetObjects
open Testing

let newNodes = [
    createNode "path" "one"
    createNode "path" "two"
]

let createModel () =
    let model = createBaseTestModel()
    model.Nodes <- [
        createNode "old" "old one"
        createNode "old" "old two"
        createNode "old" "old three"
    ]
    model.Path <- Path "old"
    model.Cursor <- 2
    model

let createController () =
    let fileSys =
        Mock<IFileSystemService>()
            .Setup(fun x -> <@ x.Normalize(Path "path") @>).Returns(Path "normalized")
            .Setup(fun x -> <@ x.GetNodes(Path "normalized") @>).Returns(newNodes)
            .Setup(fun x -> <@ x.Normalize(Path "bad path") @>).Returns(Path "bad normalized")
            .Setup(fun x -> <@ x.GetNodes(Path "bad normalized") @>).Raises<UnauthorizedAccessException>()
            .Create()
    let settingsFactory () = Mock.Of<Mvc<SettingsEvents, SettingsModel>>()
    MainController(fileSys, settingsFactory)

[<Test>]
let ``Opening a valid path updates model correctly``() =
    let model = createModel()
    let contr = createController()
    contr.OpenPath (Path "path") 1 model

    let expected = createModel()
    expected.Nodes <- newNodes
    expected.Path <- Path "normalized"
    expected.Cursor <- 1
    expected.BackStack <- (Path "old", 2) :: expected.BackStack
    expected.ForwardStack <- []
    assertAreEqual expected model

[<Test>]
let ``Opening a path that throws on GetNodes sets error status only``() =
    let model = createModel()
    let contr = createController()
    contr.OpenPath (Path "bad path") 0 model

    let expected = createModel()
    expected.IsErrorStatus <- true
    CompareLogic()
        |> ignoreMembers ["Status"]
        |> assertAreEqualWith expected model
    model.Status |> shouldContainText "Could not open path"
    model.Status |> shouldContainText "unauthorized"
