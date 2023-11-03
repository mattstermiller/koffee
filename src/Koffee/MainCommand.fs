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
    let cursor = CursorToItem (model.CursorItem, false)
    match model.SearchCurrent |> Option.bind (Search.getFilter model.Config.ShowHidden >> Result.toOption) with
    | Some filter ->
        let items =
            model.Directory @ (model.SubDirectories |? [])
            |> filter
            |> (model.Sort |> Option.map SortField.SortByTypeThen |? id)
            |> model.ItemsOrEmpty
        { model with Items = items } |> Nav.moveCursor cursor
    | None ->
        Nav.listDirectory cursor model

let private getDropInPutType (event: DragEvent) (model: MainModel) (path: Path) =
    let desiredPutType =
        event.PutType |> Option.defaultWith (fun () -> if path.Base = model.Location.Base then Move else Copy)
    event.AllowedPutTypes
    |> List.tryFind ((=) desiredPutType)
    |> Option.orElse (event.AllowedPutTypes |> List.tryHead)

let updateDropInPutType (paths: Path list) (event: DragEvent) (model: MainModel) =
    event.PutType <- paths |> List.tryHead |> Option.bind (getDropInPutType event model)
    model

let dropIn (fs: IFileSystem) progress paths (event: DragEvent) (model: MainModel) = asyncSeqResult {
    // TODO: support multiple items
    match getDropInPutType event model (paths |> List.head) with
    | Some putType ->
        let paths =
            if putType = Move then
                paths |> List.filter (fun p -> p.Parent <> model.Location)
            else
                paths
        match paths |> List.tryHead with
        | Some path ->
            match! fs.GetItem path |> actionError "drop item" with
            | Some item ->
                yield! Action.putInLocation fs progress false false putType item model
            | None -> ()
        | None -> ()
    | None -> ()
}

let dropOut (fsReader: IFileSystemReader) putType (model: MainModel) =
    // TODO: move to MainAction, call removeItem
    // TODO: support multiple items
    if putType = Move && fsReader.GetItem model.CursorItem.Path = Ok None then
        let items = model.Items |> List.except [model.CursorItem] |> model.ItemsOrEmpty
        { model with Items = items }
    else
        model

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

let openExplorer (os: IOperatingSystem) (model: MainModel) =
    // TODO: support multiple items
    os.OpenExplorer model.ActionItems.Head
    model |> MainModel.withMessage MainStatus.OpenExplorer

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
    // TODO: support multiple items
    let item = model.ActionItems.Head
    match item.Type with
    // TODO: support open properties for Drive, others?
    | File | Folder ->
        do! os.OpenProperties item.Path |> actionError "open properties"
        return model |> MainModel.withMessage (MainStatus.OpenProperties item.Name)
    | _ ->
        return model
}

let openCommandLine (os: IOperatingSystem) model = result {
    if model.Location <> Path.Root then
        do! os.LaunchApp model.Config.CommandlinePath model.Location ""
            |> Result.mapError (fun e -> MainStatus.CouldNotOpenApp ("Commandline tool", e))
        return model |> MainModel.withMessage (MainStatus.OpenCommandLine model.LocationFormatted)
    else return model
}

let openWithTextEditor (os: IOperatingSystem) (model: MainModel) = result {
    // TODO: support multiple items
    let item = model.ActionItems.Head
    match item.Type with
    | File ->
        let args = item.Path.Format Windows |> sprintf "\"%s\""
        do! os.LaunchApp model.Config.TextEditor model.Location args
            |> Result.mapError (fun e -> MainStatus.CouldNotOpenApp ("Text Editor", e))
        return
            model
            |> MainModel.mapHistory (History.withFilePaths model.Config.Limits.PathHistory [item.Path])
            |> MainModel.withMessage (MainStatus.OpenTextEditor item.Name)
    | _ -> return model
}

let openSettings fsReader openSettings model = result {
    let config = openSettings model.Config
    return! { model with Config = config } |> Nav.refresh fsReader
}
