module Koffee.Main.Command

open Acadian.FSharp
open Koffee
open Koffee.Main.Util

let toggleHidden (model: MainModel) =
    let show = not model.Config.ShowHidden
    let model =
        { model with
            Config = { model.Config with ShowHidden = show }
        } |> MainModel.withMessage (MainStatus.ToggleHidden show)
    match model.SearchCurrent |> Option.bind (Search.getFilter model.Config.ShowHidden >> Result.toOption) with
    | Some filter ->
        let items =
            model.Directory @ (model.SubDirectories |? [])
            |> filter
            |> model.SortItems
            |> model.ItemsOrEmpty
        { model with Items = items } |> Nav.moveCursor model.KeepCursorByPath
    | None ->
        model |> Nav.listDirectory model.KeepCursorByPath

let openSplitScreenWindow (os: IOperatingSystem) getScreenBounds model = result {
    let fitRect = Rect.ofPairs model.WindowLocation (model.WindowSize |> mapFst ((*) 2))
                  |> Rect.fit (getScreenBounds())
    let model =
        { model with
            WindowLocation = fitRect.Location
            WindowSize = fitRect.Size |> mapFst (flip (/) 2)
        }

    let left, top = model.WindowLocation
    let width, height = model.WindowSize
    let path = (model.Location.Format Windows).TrimEnd([|'\\'|])
    let args = sprintf "\"%s\" --location=%i,%i --size=%i,%i"
                       path (left + width) top width height

    let! koffeePath = Path.Parse (System.Reflection.Assembly.GetExecutingAssembly().Location)
                      |> Result.ofOption MainStatus.CouldNotFindKoffeeExe
    let folder = koffeePath.Parent
    do! os.LaunchApp (koffeePath.Format Windows) folder args
        |> Result.mapError (fun e -> MainStatus.CouldNotOpenApp ("Koffee", e))
    return model
}

let openFileWith (os: IOperatingSystem) (model: MainModel) = result {
    match model.ActionItems with
    | [item] when item.Type = File ->
        do! os.OpenFileWith item.Path |> actionError "open file with"
        return model |> MainModel.withMessage (MainStatus.OpenFiles [item.Name])
    | [_] ->
        return model
    | _ ->
        return! Error MainStatus.CannotOpenWithMultiple
}

let openProperties (os: IOperatingSystem) (model: MainModel) = result {
    let items =
        model.ActionItems |> List.filter (fun i ->
            i.Type |> Seq.containedIn [File; Folder; Drive; NetShare]
            && not (i.Path = Path.Network)
        )
    if items.IsEmpty then
        return model
    else
        do! os.OpenProperties (items |> Seq.map (fun i -> i.Path)) |> actionError "open properties"
        return model |> MainModel.withMessage (MainStatus.OpenProperties (items |> List.map (fun i -> i.Name)))
}

let openWithTextEditor (os: IOperatingSystem) (model: MainModel) = result {
    let items = model.ActionItems |> List.filter (fun i -> i.Type = File)
    if items.IsEmpty then
        return model
    else
        let pathArg (path: Path) = path.Format Windows |> sprintf "\"%s\""
        let paths = items |> List.map (fun i -> i.Path)
        let args = paths |> Seq.map pathArg |> String.concat " "
        do! os.LaunchApp model.Config.TextEditor model.Location args
            |> Result.mapError (fun e -> MainStatus.CouldNotOpenApp ("Text editor", e))
        return
            model
            |> MainModel.mapHistory (History.withFilePaths model.Config.Limits.PathHistory paths)
            |> MainModel.withMessage (MainStatus.OpenTextEditor (items |> List.map (fun i -> i.Name)))
}

let openTerminal (os: IOperatingSystem) model = result {
    if model.Location <> Path.Root then
        do! os.LaunchApp model.Config.TerminalPath model.Location ""
            |> Result.mapError (fun e -> MainStatus.CouldNotOpenApp ("Terminal", e))
        return model |> MainModel.withMessage (MainStatus.OpenTerminal model.Location)
    else return model
}

let openExplorer (os: IOperatingSystem) (model: MainModel) = result {
    let parent = model.ActionItems.Head.Path.Parent
    let selectPaths =
        model.ActionItems
        |> Seq.filter (fun i -> i.Path.Parent = parent)
        |> Seq.map (fun i -> i.Path)
    do! os.OpenExplorer parent selectPaths |> actionError "open Explorer"
    return model |> MainModel.withMessage MainStatus.OpenExplorer
}

let openSettings fsReader openSettings model = result {
    let config = openSettings model.Config
    return! { model with Config = config } |> Nav.refresh fsReader
}
