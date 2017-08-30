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

let comparer () =
    CompareLogic() |> ignoreMembers ["CommandText"]

[<Test>]
let ``Create folder calls fsCreate, openPath and sets status``() =
    let mutable created : (NodeType * Path) option = None
    let fsCreate nodeType path = created <- Some (nodeType, path)
    let openPath path select (model: MainModel) =
        model.Nodes <- newNodes
        model.Cursor <- 1
    let createNode = newNodes.[1]
    let model = createModel()
    MainLogic.create fsCreate openPath Folder createNode.Name model

    created |> shouldEqual (Some (createNode.Type, createNode.Path))
    let expectedAction = CreatedItem createNode
    let expected = createModel()
    expected.Nodes <- newNodes
    expected.Cursor <- 1
    expected.UndoStack <- expectedAction :: expected.UndoStack
    expected.RedoStack <- []
    expected.Status <- Some <| MainStatus.actionComplete expectedAction model.PathFormat
    assertAreEqualWith expected model (comparer())

[<Test>]
let ``Create folder handles error by setting error status``() =
    let fsCreate nodeType path = raise ex
    let openPath path select (model: MainModel) = failwith "this should not be called"
    let createNode = newNodes.[1]
    let model = createModel()
    MainLogic.create fsCreate openPath Folder createNode.Name model

    let expected = createModel()
    expected |> MainStatus.setActionExceptionStatus (CreatedItem createNode) ex
    assertAreEqualWith expected model (comparer())

[<Test>]
let ``Rename calls fsMove, openPath and sets status``() =
    let mutable renamed : (Path * Path) option = None
    let fsMove path newPath = renamed <- Some (path, newPath)
    let openPath path select (model: MainModel) =
        model.Nodes <- newNodes
        model.Cursor <- 1
    let newName = newNodes.[1].Name
    let model = createModel()
    model.Cursor <- 1
    MainLogic.rename fsMove openPath oldNodes.[1] newName model

    renamed |> shouldEqual (Some (oldNodes.[1].Path, newNodes.[1].Path))
    let expectedAction = RenamedItem (oldNodes.[1], newName)
    let expected = createModel()
    expected.Nodes <- newNodes
    expected.Cursor <- 1
    expected.UndoStack <- expectedAction :: expected.UndoStack
    expected.RedoStack <- []
    expected.Status <- Some <| MainStatus.actionComplete expectedAction model.PathFormat
    assertAreEqualWith expected model (comparer())

[<Test>]
let ``Rename handles error by setting error status``() =
    let fsMove path newPath = raise ex
    let openPath path select (model: MainModel) = failwith "this should not be called"
    let newName = newNodes.[1].Name
    let model = createModel()
    model.Cursor <- 1
    MainLogic.rename fsMove openPath oldNodes.[1] newName model

    let expected = createModel()
    expected.Cursor <- 1
    expected |> MainStatus.setActionExceptionStatus (RenamedItem (oldNodes.[1], newNodes.[1].Name)) ex
    assertAreEqualWith expected model (comparer())


let renameTextSelection cursorPosition fileName =
    let node = createNode "path" fileName
    let model = createModel()
    model.Nodes <- List.append oldNodes [node]
    model.Cursor <- model.Nodes.Length - 1
    MainLogic.startInput (Rename cursorPosition) model

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
let ``StartInput for rename replace all sets command text and selection``() =
    renameTextSelection ReplaceAll "three.txt.old" |> shouldEqual (0, 13)

[<Test>]
let ``StartInput for rename replace all with no extension sets command text and selection``() =
    renameTextSelection ReplaceAll "three" |> shouldEqual (0, 5)

[<Test>]
let ``StartInput for rename replace all with just dot sets command text and selection``() =
    renameTextSelection ReplaceAll "three." |> shouldEqual (0, 6)
