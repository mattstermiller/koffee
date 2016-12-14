namespace Koffee.Tests

open System
open System.Windows.Input
open FSharp.Desktop.UI
open NUnit.Framework
open FsUnit
open FsUnitTyped
open Foq
open Koffee

[<TestFixture>]
type ``MainController tests for creating and renaming files and folders``() =
    let oldNodes = [
        {Path = Path "path1"; Name = "one"; Type = Folder; Modified = None; Size = None}
        {Path = Path "path2"; Name = "two"; Type = Folder; Modified = None; Size = None}
    ]

    let newNodes = [
        {Path = Path "path/new one"; Name = "new one"; Type = Folder; Modified = None; Size = None}
        {Path = Path "path/new two"; Name = "new two"; Type = Folder; Modified = None; Size = None}
    ]

    let CreateModel () =
        let model = Model.Create<MainModel>()
        model.Path <- Path "path"
        model.Nodes <- oldNodes
        model.Cursor <- 0
        model.CommandTextSelection <- (1, 1)
        model.BackStack <- [Path "back", 8]
        model.ForwardStack <- [Path "fwd", 9]
        model

    let AssertModelDidNotChangeNavHistory (model: MainModel) =
        model.BackStack |> should equal [Path "back", 8]
        model.ForwardStack |> should equal [Path "fwd", 9]

    let CreateFileSys () =
        Mock<IFileSystemService>()
            .Setup(fun x -> <@ x.GetNodes (Path "path") @>).Returns(newNodes)
            .Create()

    let CreateUnauthorizedFileSys () =
        let path = Path "path"
        Mock<IFileSystemService>()
            .Setup(fun x -> <@ x.GetNodes path @>).Returns(newNodes)
            .Setup(fun x -> <@ x.CreateFolder path (any()) @>).Raises<UnauthorizedAccessException>()
            .Setup(fun x -> <@ x.Rename (any()) (any()) @>).Raises<UnauthorizedAccessException>()
            .Create()

    let CreateController fileSys =
        let settingsFactory () = Mock.Of<Mvc<SettingsEvents, SettingsModel>>()
        MainController(fileSys, settingsFactory)

    let startRenameTextSelection cursorPosition fileName =
        let fileSys = CreateFileSys()
        let contr = CreateController fileSys
        let model = CreateModel()
        let node = {Path = Path "path3"; Name = fileName; Type = File; Modified = None; Size = None}
        model.Nodes <- List.append oldNodes [node]
        model.Cursor <- model.Nodes.Length - 1
        contr.StartInput (Rename cursorPosition) model

        model.CommandInputMode |> should equal (Some (Rename cursorPosition))
        model.CommandText |> should equal node.Name
        model.CommandTextSelection


    [<Test>]
    member x.``Create folder calls fileSys.CreateFolder, reloads nodes and sets cursor``() =
        let fileSys = CreateFileSys()
        let contr = CreateController fileSys
        let model = CreateModel()
        model.CommandInputMode <- Some CreateFolder
        model.CommandText <- "new two"
        contr.ExecuteCommand model

        let path = (Path "path")
        verify <@ fileSys.CreateFolder path "new two" @> once
        model.CommandInputMode |> should equal None
        model.Nodes |> should equal newNodes
        model.Cursor |> should equal 1
        model.Status |> should equal "Created folder: new two"
        AssertModelDidNotChangeNavHistory model

    [<Test>]
    member x.``Create folder handles error by setting error status``() =
        let fileSys = CreateUnauthorizedFileSys()
        let contr = CreateController fileSys
        let model = CreateModel()
        model.CommandInputMode <- Some CreateFolder
        model.CommandText <- "new two"
        contr.ExecuteCommand model

        model.CommandInputMode |> should equal None
        model.Nodes |> should equal oldNodes
        model.Cursor |> should equal 0
        model.Status |> should startWith "Could not create folder new two"
        model.IsErrorStatus |> should equal true
        AssertModelDidNotChangeNavHistory model

    [<Test>]
    member x.``Rename calls fileSys.Rename, reloads nodes and sets cursor``() =
        let fileSys = CreateFileSys()
        let contr = CreateController fileSys
        let model = CreateModel()
        model.Cursor <- 1
        model.CommandInputMode <- Some (Rename Begin)
        model.CommandText <- "new two"
        contr.ExecuteCommand model

        let node = oldNodes.[1]
        verify <@ fileSys.Rename node "new two" @> once
        model.CommandInputMode |> should equal None
        model.Nodes |> should equal newNodes
        model.Cursor |> should equal 1
        model.Status |> should equal "Renamed two to: new two"
        AssertModelDidNotChangeNavHistory model

    [<Test>]
    member x.``Rename handles error by setting error status``() =
        let fileSys = CreateUnauthorizedFileSys()
        let contr = CreateController fileSys
        let model = CreateModel()
        model.Cursor <- 1
        model.CommandInputMode <- Some (Rename Begin)
        model.CommandText <- "new two"
        contr.ExecuteCommand model

        model.CommandInputMode |> should equal None
        model.Nodes |> should equal oldNodes
        model.Cursor |> should equal 1
        model.Status |> should startWith "Could not rename two"
        model.IsErrorStatus |> should equal true
        AssertModelDidNotChangeNavHistory model

    [<Test>]
    member x.``StartInput for rename at beginning sets command text and selection``() =
        startRenameTextSelection Begin "three.txt.old" |> should equal (0, 0)

    [<Test>]
    member x.``StartInput for rename at end sets command text and selection``() =
        startRenameTextSelection End "three.txt.old" |> should equal (9, 0)

    [<Test>]
    member x.``StartInput for rename replace sets command text and selection``() =
        startRenameTextSelection Replace "three.txt.old" |> should equal (0, 9)
