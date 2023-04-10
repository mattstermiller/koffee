module Koffee.Main.Command

open Acadian.FSharp
open Koffee
open Koffee.Main.Util

let toggleHidden (model: MainModel) =
    let show = not model.Config.ShowHidden
    let model =
        { model with
            Config = { model.Config with ShowHidden = show }
        } |> fun m -> m.WithMessage (MainStatus.ToggleHidden show)
    let select = SelectItem (model.SelectedItem, false)
    match model.SearchCurrent |> Option.bind (Search.getFilter model.Config.ShowHidden >> Result.toOption) with
    | Some filter ->
        let items =
            model.Directory @ (model.SubDirectories |? [])
            |> filter
            |> (model.Sort |> Option.map SortField.SortByTypeThen |? id)
            |> model.ItemsOrEmpty
        { model with Items = items } |> Nav.select select
    | None ->
        Nav.listDirectory select model

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
    match getDropInPutType event model (paths |> List.head) with
    | Some putType ->
        let paths =
            if putType = Move then
                paths |> List.filter (fun p -> p.Parent <> model.Location)
            else
                paths
        // only supports one item for now until multi-select is implemented
        match paths |> List.tryHead with
        | Some path ->
            match! fs.GetItem path |> actionError "drop item" with
            | Some item ->
                yield! Action.putItem fs progress false item putType model
            | None -> ()
        | None -> ()
    | None -> ()
}

let dropOut (fsReader: IFileSystemReader) putType (model: MainModel) =
    if putType = Move && fsReader.GetItem model.SelectedItem.Path = Ok None then
        let items = model.Items |> List.except [model.SelectedItem] |> model.ItemsOrEmpty
        { model with Items = items }
    else
        model

let openSplitScreenWindow (os: IOperatingSystem) getScreenBounds model = result {
    let fitRect = Rect.ofPairs model.WindowLocation (model.WindowSize |> fstf ((*) 2))
                  |> Rect.fit (getScreenBounds())
    let model =
        { model with
            WindowLocation = fitRect.Location
            WindowSize = fitRect.Size |> fstf (flip (/) 2)
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
    os.OpenExplorer model.SelectedItem
    model.WithMessage MainStatus.OpenExplorer

let openFileWith (os: IOperatingSystem) (model: MainModel) = result {
    let item = model.SelectedItem
    match item.Type with
    | File ->
        do! os.OpenFileWith item.Path |> actionError "open file with"
        return model.WithMessage (MainStatus.OpenFile item.Name)
    | _ ->
        return model
}

let openProperties (os: IOperatingSystem) (model: MainModel) = result {
    let item = model.SelectedItem
    match item.Type with
    | File | Folder ->
        do! os.OpenProperties item.Path |> actionError "open properties"
        return model.WithMessage (MainStatus.OpenProperties item.Name)
    | _ ->
        return model
}

let openCommandLine (os: IOperatingSystem) model = result {
    if model.Location <> Path.Root then
        do! os.LaunchApp model.Config.CommandlinePath model.Location ""
            |> Result.mapError (fun e -> MainStatus.CouldNotOpenApp ("Commandline tool", e))
        return model.WithMessage (MainStatus.OpenCommandLine model.LocationFormatted)
    else return model
}

let openWithTextEditor (os: IOperatingSystem) (model: MainModel) = result {
    match model.SelectedItem.Type with
    | File ->
        let args = model.SelectedItem.Path.Format Windows |> sprintf "\"%s\""
        do! os.LaunchApp model.Config.TextEditor model.Location args
            |> Result.mapError (fun e -> MainStatus.CouldNotOpenApp ("Text Editor", e))
        return
            { model with
                History = model.History.WithFilePath model.Config.Limits.PathHistory model.SelectedItem.Path
            }.WithMessage (MainStatus.OpenTextEditor model.SelectedItem.Name)
    | _ -> return model
}

let openSettings fsReader openSettings model = result {
    let config = openSettings model.Config
    return! { model with Config = config } |> Nav.refresh fsReader
}
