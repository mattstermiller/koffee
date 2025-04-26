module Koffee.ItemActionCommands.Rename

open Acadian.FSharp
open Koffee

let private getInputSelection renamePart itemType (input: string) =
    let fullLen = input.Length
    let nameLen =
        if itemType = File
        then Path.SplitName input |> fst |> String.length
        else fullLen
    match renamePart with
    | Begin -> (0, 0)
    | EndName -> (nameLen, 0)
    | End -> (fullLen, 0)
    | ReplaceName -> (0, nameLen)
    | ReplaceAll -> (0, fullLen)

let inputRename part (model: MainModel) =
    let item = model.CursorItem
    if item.Type.CanModify then
        { model with
            InputMode = Some (Input (Rename part))
            InputText = item.Name
            InputTextSelection = getInputSelection part item.Type item.Name
        }
    else
        model

let rename (fs: IFileSystem) item newName (model: MainModel) = result {
    if item.Type.CanModify then
        let action = RenamedItem (item, newName)
        let newPath = item.Path.Parent.Join newName
        let! existing =
            if String.equalsIgnoreCase item.Name newName
            then Ok None
            else fs.GetItem newPath |> itemActionError action
        match existing with
        | None ->
            do! fs.Move item.Type item.Path newPath |> itemActionError action
            let newItem = { item with Name = newName; Path = newPath }
            let substitute = List.map (fun i -> if i = item then newItem else i)
            return
                { model with
                    Directory = model.Directory |> substitute
                    History = model.History |> History.withPathReplaced item.Path newPath
                }
                |> fun model ->
                    if model.SearchCurrent.IsSome
                    then { model with Items = model.Items |> substitute }
                    else NavigationCommands.listDirectory (CursorToPath (newPath, false)) model
                |> performedAction action
        | Some existingItem ->
            return! Error <| MainStatus.CannotUseNameAlreadyExists ("rename", item.Type, newName, existingItem.IsHidden)
    else return model
}

let undoRename (fs: IFileSystem) undoIter oldItem currentName (model: MainModel) = result {
    let parentPath = oldItem.Path.Parent
    let currentPath = parentPath.Join currentName
    let item = { oldItem with Name = currentName; Path = currentPath }
    let action = RenamedItem (item, oldItem.Name)
    let! existing =
        if String.equalsIgnoreCase oldItem.Name currentName then Ok None
        else fs.GetItem oldItem.Path |> itemActionError action
    match existing with
    | None ->
        do! fs.Move oldItem.Type currentPath oldItem.Path |> itemActionError action
        return
            model
            |> MainModel.mapHistory (History.withPathReplaced item.Path oldItem.Path)
            |> performedUndo undoIter (RenamedItem (oldItem, currentName))
            |> NavigationCommands.openPathIgnoreError fs parentPath (CursorToPath (oldItem.Path, false))
    | Some existingItem ->
        return! Error <| MainStatus.CannotUseNameAlreadyExists ("rename", oldItem.Type, oldItem.Name, existingItem.IsHidden)
}
