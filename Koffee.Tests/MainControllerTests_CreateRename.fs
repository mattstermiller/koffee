module Koffee.MainControllerTests_CreateRename

open System
open System.Windows.Input
open FSharp.Desktop.UI
open NUnit.Framework
open FsUnitTyped
open Foq
open KellermanSoftware.CompareNetObjects
open Testing

let oldNodes = [
    createNode "path" "one"  
    createNode "path" "two"
]

let newNodes = [
    createNode "path" "new one"
    createNode "path" "new two"
]

let createModel () =
    let model = createBaseTestModel()
    model.Path <- Path "path"
    model.Nodes <- oldNodes
    model.Cursor <- 0
    model.CommandText <- ""
    model.CommandTextSelection <- (1, 1)
    model

let ex = UnauthorizedAccessException()

let createFileSys () =
    (baseFileSysMock newNodes).Create()

let createUnauthorizedFileSys () =
    (baseFileSysMock newNodes)
        .Setup(fun x -> <@ x.Create (any()) (any()) @>).Raises(ex)
        .Setup(fun x -> <@ x.Rename (any()) (any()) @>).Raises(ex)
        .Create()

let createController fileSys =
    let settingsFactory () = Mock.Of<Mvc<SettingsEvents, SettingsModel>>()
    MainController(fileSys, settingsFactory)

let comparer () =
    CompareLogic() |> ignoreMembers ["CommandText"]

[<Test>]
let ``Create folder calls fileSys.Create, reloads nodes and sets cursor``() =
    let fileSys = createFileSys()
    let contr = createController fileSys
    let createNode = newNodes.[1]
    let model = createModel()
    model.CommandInputMode <- Some CreateFolder
    model.CommandText <- createNode.Name
    contr.ExecuteCommand model

    let nodeType = createNode.Type
    let path = createNode.Path
    verify <@ fileSys.Create nodeType path @> once
    let expected = createModel()
    expected.Nodes <- newNodes
    expected.Cursor <- 1
    expected.UndoStack <- CreatedItem createNode :: expected.UndoStack
    expected.RedoStack <- []
    expected.Status <- MainController.ActionStatus (CreatedItem createNode)
    comparer() |> assertAreEqualWith expected model

[<Test>]
let ``Create folder handles error by setting error status``() =
    let fileSys = createUnauthorizedFileSys()
    let contr = createController fileSys
    let createNode = newNodes.[1]
    let model = createModel()
    model.CommandInputMode <- Some CreateFolder
    model.CommandText <- createNode.Name
    contr.ExecuteCommand model

    let expected = createModel()
    expected |> MainController.SetActionExceptionStatus (CreatedItem createNode) ex
    comparer() |> assertAreEqualWith expected model

[<Test>]
let ``Rename calls fileSys.Rename, reloads nodes and sets cursor``() =
    let fileSys = createFileSys()
    let contr = createController fileSys
    let newName = newNodes.[1].Name
    let model = createModel()
    model.Cursor <- 1
    model.CommandInputMode <- Some (Rename Begin)
    model.CommandText <- newName
    contr.ExecuteCommand model

    let nodeType = Folder
    let path = oldNodes.[1].Path
    verify <@ fileSys.Rename nodeType path newName @> once
    let expected = createModel()
    expected.Nodes <- newNodes
    expected.Cursor <- 1
    expected.UndoStack <- RenamedItem (oldNodes.[1], newName) :: expected.UndoStack
    expected.RedoStack <- []
    expected.Status <- MainController.ActionStatus (RenamedItem (oldNodes.[1], newName))
    comparer() |> assertAreEqualWith expected model

[<Test>]
let ``Rename handles error by setting error status``() =
    let fileSys = createUnauthorizedFileSys()
    let contr = createController fileSys
    let model = createModel()
    model.Cursor <- 1
    model.CommandInputMode <- Some (Rename Begin)
    model.CommandText <- newNodes.[1].Name
    contr.ExecuteCommand model

    let expected = createModel()
    expected.Cursor <- 1
    expected |> MainController.SetActionExceptionStatus (RenamedItem (oldNodes.[1], newNodes.[1].Name)) ex
    comparer() |> assertAreEqualWith expected model


let renameTextSelection cursorPosition fileName =
    let fileSys = createFileSys()
    let contr = createController fileSys
    let model = createModel()
    let node = {Path = Path "path3"; Name = fileName; Type = File; Modified = None; Size = None}
    model.Nodes <- List.append oldNodes [node]
    model.Cursor <- model.Nodes.Length - 1
    contr.StartInput (Rename cursorPosition) model

    model.CommandInputMode |> shouldEqual (Some (Rename cursorPosition))
    model.CommandText |> shouldEqual node.Name
    model.CommandTextSelection

[<Test>]
let ``StartInput for rename at beginning sets command text and selection``() =
    renameTextSelection Begin "three.txt.old" |> shouldEqual (0, 0)

[<Test>]
let ``StartInput for rename at end sets command text and selection``() =
    renameTextSelection End "three.txt.old" |> shouldEqual (9, 0)

[<Test>]
let ``StartInput for rename replace sets command text and selection``() =
    renameTextSelection Replace "three.txt.old" |> shouldEqual (0, 9)
