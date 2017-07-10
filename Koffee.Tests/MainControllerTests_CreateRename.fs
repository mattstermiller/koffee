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
    model.Path <- createPath "path"
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
        .Setup(fun x -> <@ x.Move (any()) (any()) @>).Raises(ex)
        .Create()

let createController fileSys =
    let settingsFactory () = Mock.Of<Mvc<SettingsEvents, SettingsModel>>()
    MainController(fileSys, settingsFactory, Config(), None)

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
    let expectedAction = CreatedItem createNode
    let expected = createModel()
    expected.Nodes <- newNodes
    expected.Cursor <- 1
    expected.UndoStack <- expectedAction :: expected.UndoStack
    expected.RedoStack <- []
    expected.Status <- Some <| MainStatus.actionComplete expectedAction model.PathFormat
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
    expected |> MainStatus.setActionExceptionStatus (CreatedItem createNode) ex
    comparer() |> assertAreEqualWith expected model

[<Test>]
let ``Rename calls fileSys.Move, reloads nodes and sets cursor``() =
    let fileSys = createFileSys()
    let contr = createController fileSys
    let newName = newNodes.[1].Name
    let model = createModel()
    model.Cursor <- 1
    model.CommandInputMode <- Some (Rename Begin)
    model.CommandText <- newName
    contr.ExecuteCommand model

    let oldPath = oldNodes.[1].Path
    let newPath = newNodes.[1].Path
    verify <@ fileSys.Move oldPath newPath @> once
    let expectedAction = RenamedItem (oldNodes.[1], newName)
    let expected = createModel()
    expected.Nodes <- newNodes
    expected.Cursor <- 1
    expected.UndoStack <- expectedAction :: expected.UndoStack
    expected.RedoStack <- []
    expected.Status <- Some <| MainStatus.actionComplete expectedAction model.PathFormat
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
    expected |> MainStatus.setActionExceptionStatus (RenamedItem (oldNodes.[1], newNodes.[1].Name)) ex
    comparer() |> assertAreEqualWith expected model


let renameTextSelection cursorPosition fileName =
    let fileSys = createFileSys()
    let contr = createController fileSys
    let model = createModel()
    let node = {Path = createPath "path3"; Name = fileName; Type = File; Modified = None; Size = None}
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
let ``StartInput for rename at end of name sets command text and selection``() =
    renameTextSelection EndName "three.txt.old" |> shouldEqual (9, 0)

[<Test>]
let ``StartInput for rename at end of full name sets command text and selection``() =
    renameTextSelection End "three.txt.old" |> shouldEqual (13, 0)

[<Test>]
let ``StartInput for rename replace name sets command text and selection``() =
    renameTextSelection ReplaceName "three.txt.old" |> shouldEqual (0, 9)

[<Test>]
let ``StartInput for rename replace name with no name sets command text and selection``() =
    renameTextSelection ReplaceName ".txt" |> shouldEqual (0, 0)

[<Test>]
let ``StartInput for rename replace extension sets command text and selection``() =
    renameTextSelection ReplaceExt "three.txt.old" |> shouldEqual (10, 3)

[<Test>]
let ``StartInput for rename replace extension with no extension sets command text and selection``() =
    renameTextSelection ReplaceExt "three" |> shouldEqual (5, 0)

[<Test>]
let ``StartInput for rename replace extension with just dot sets command text and selection``() =
    renameTextSelection ReplaceExt "three." |> shouldEqual (6, 0)
