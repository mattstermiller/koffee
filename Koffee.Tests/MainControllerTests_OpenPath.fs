module Koffee.MainControllerTests_OpenPath

open System
open System.Windows.Input
open FSharp.Desktop.UI
open NUnit.Framework
open FsUnitTyped
open Foq
open KellermanSoftware.CompareNetObjects
open Testing

let oldNodes = [
    createNode "old" "old one"
    createNode "old" "old two"
    createNode "old" "old three"
]

let newNodes = [
    createNode "path" "one"
    createNode "path" "two"
]

let createModel () =
    let model = createBaseTestModel()
    model.Nodes <- oldNodes
    model.Path <- createPath "old"
    model.Cursor <- 2
    model

let ex = UnauthorizedAccessException()

let createController () =
    let fileSys =
        Mock<IFileSystemService>()
            .Setup(fun x -> <@ x.GetNodes(createPath "path") @>).Returns(newNodes)
            .Setup(fun x -> <@ x.GetNodes(createPath "old") @>).Returns(oldNodes)
            .Setup(fun x -> <@ x.GetNodes(createPath "bad path") @>).Raises(ex)
            .Create()
    let settingsFactory () = Mock.Of<Mvc<SettingsEvents, SettingsModel>>()
    MainController(fileSys, settingsFactory)

[<Test>]
let ``Opening a valid path updates model correctly``() =
    let model = createModel()
    let contr = createController()
    contr.OpenPath (createPath "path") 1 model

    let expected = createModel()
    expected.Nodes <- newNodes
    expected.Path <- createPath "path"
    expected.Cursor <- 1
    expected.BackStack <- (createPath "old", 2) :: expected.BackStack
    expected.ForwardStack <- []
    assertAreEqual expected model

[<Test>]
let ``Opening same path does not modify navigation history``() =
    let model = createModel()
    let contr = createController()
    contr.OpenPath (createPath "old") 1 model

    let expected = createModel()
    expected.Cursor <- 1
    assertAreEqual expected model

[<Test>]
let ``Opening a path that throws on GetNodes sets error status only``() =
    let model = createModel()
    let contr = createController()
    contr.OpenPath (createPath "bad path") 0 model

    let expected = createModel()
    expected.SetExceptionStatus ex "open path"
    assertAreEqual expected model
