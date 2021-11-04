module Koffee.Main.Command

open Acadian.FSharp
open Koffee
open Koffee.Main.Util

let toggleHidden (model: MainModel) =
    let show = not model.Config.ShowHidden
    let model =
        { model with
            Config = { model.Config with ShowHidden = show }
        } |> fun m -> m.WithStatus (MainStatus.toggleHidden show)
    let select = SelectItem (model.SelectedItem, false)
    match model.SearchCurrent |> Option.bind (Search.getFilter model.Config.ShowHidden) with
    | Some filter ->
        let items =
            model.Directory @ (model.SubDirectories |? [])
            |> filter
            |> (model.Sort |> Option.map SortField.SortByTypeThen |? id)
            |> model.ItemsIfEmpty
        { model with Items = items } |> Nav.select select
    | None ->
        Nav.listDirectory select model

let private getDropInAction (event: DragEvent) (model: MainModel) (path: Path) =
    let desiredAction =
        event.Action |> Option.defaultWith (fun () -> if path.Base = model.Location.Base then Move else Copy)
    event.AllowedActions
    |> List.tryFind ((=) desiredAction)
    |> Option.orElse (event.AllowedActions |> List.tryHead)

let updateDropInAction (paths: Path list) (event: DragEvent) (model: MainModel) =
    event.Action <- paths |> List.tryHead |> Option.bind (getDropInAction event model)
    model

let dropIn (fs: IFileSystem) paths (event: DragEvent) (model: MainModel) = asyncSeqResult {
    match getDropInAction event model (paths |> List.head) with
    | Some action ->
        let paths =
            if action = Move then
                paths |> List.filter (fun p -> p.Parent <> model.Location)
            else
                paths
        // only supports one item for now until multi-select is implemented
        match paths |> List.tryHead with
        | Some path ->
            match! fs.GetItem path |> actionError "drop item" with
            | Some item ->
                yield! Action.putItem fs false item action model
            | None -> ()
        | None -> ()
    | None -> ()
}

let dropOut (fsReader: IFileSystemReader) dropAction (model: MainModel) =
    if dropAction = Move && fsReader.GetItem model.SelectedItem.Path = Ok None then
        let items = model.Items |> List.except [model.SelectedItem] |> model.ItemsIfEmpty
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
                      |> Result.ofOption CouldNotFindKoffeeExe
    let folder = koffeePath.Parent
    do! os.LaunchApp (koffeePath.Format Windows) folder args
        |> Result.mapError (fun e -> CouldNotOpenApp ("Koffee", e))
    return model
}

let openExplorer (os: IOperatingSystem) (model: MainModel) =
    os.OpenExplorer model.SelectedItem
    model.WithStatusOption (Some MainStatus.openExplorer)

let openFileWith (os: IOperatingSystem) (model: MainModel) = result {
    let item = model.SelectedItem
    match item.Type with
    | File ->
        do! os.OpenFileWith item.Path |> actionError "open file with"
        return model.WithStatus (MainStatus.openFile item.Name)
    | _ ->
        return model
}

let openProperties (os: IOperatingSystem) (model: MainModel) = result {
    let item = model.SelectedItem
    match item.Type with
    | File | Folder ->
        do! os.OpenProperties item.Path |> actionError "open properties"
        return model.WithStatus (MainStatus.openProperties item.Name)
    | _ ->
        return model
}

let openCommandLine (os: IOperatingSystem) model = result {
    if model.Location <> Path.Root then
        do! os.LaunchApp model.Config.CommandlinePath model.Location ""
            |> Result.mapError (fun e -> CouldNotOpenApp ("Commandline tool", e))
        return model.WithStatus (MainStatus.openCommandLine model.LocationFormatted)
    else return model
}

let openWithTextEditor (os: IOperatingSystem) (model: MainModel) = result {
    match model.SelectedItem.Type with
    | File ->
        let args = model.SelectedItem.Path.Format Windows |> sprintf "\"%s\""
        do! os.LaunchApp model.Config.TextEditor model.Location args
            |> Result.mapError (fun e -> CouldNotOpenApp ("Text Editor", e))
        return model.WithStatus (MainStatus.openTextEditor model.SelectedItem.Name)
    | _ -> return model
}

let openSettings fsReader openSettings model = result {
    let config = openSettings model.Config
    return! { model with Config = config } |> Nav.refresh fsReader
}
