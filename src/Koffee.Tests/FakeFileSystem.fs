namespace Koffee

open System.Collections.Generic
open Acadian.FSharp

type TreeItem =
    | TreeFile of string * (Item -> Item)
    | TreeFolder of string * TreeItem list
    | TreeDrive of char * TreeItem list
with
    static member build items =
        let rec build (path: Path) items =
            items |> List.collect (fun item ->
                match item with
                | TreeFile (name, transform) ->
                    Item.Basic (path.Join name) name File |> transform |> List.singleton
                | TreeFolder (name, items) ->
                    let folder = Item.Basic (path.Join name) name Folder
                    folder :: build folder.Path items
                | TreeDrive (name, _) when path <> Path.Root ->
                    failwithf "Drive '%O' is invalid because it is not at root level" name
                | TreeDrive (name, items) ->
                    let name = string name |> String.toUpper
                    let path = "/" + name |> Path.Parse |> Option.get
                    let drive = Item.Basic path name Drive
                    drive :: build drive.Path items
            )
        if items |> List.forall (function TreeDrive _ -> true | _ -> false) then
            build Path.Root items
        else
            let path = Path.Parse "/c" |> Option.get
            Item.Basic path "C" Drive :: build path items

module FakeFileSystemErrors =
    let pathDoesNotExist (path: Path) =
        exn ("Path does not exist: " + string path)

    let pathIsNotExpectedType (path: Path) (expectedType: ItemType) (actualType: ItemType) =
        exn (sprintf "Item at path \"%O\" was expected to be type %O but was %O" path expectedType actualType)

    let notAShortcut =
        exn "Not a shortcut"

    let itemAlreadyExistsOnPath (path: Path) =
        exn ("Item already exists on path: " + string path)

    let destPathParentDoesNotExist (path: Path) =
        exn ("Destination folder does not exist: " + string path)

    let destPathParentIsNotFolder (path: Path) =
        exn ("Destination path is not a folder: " + string path)

    let cannotMoveNonEmptyFolderAcrossDrives =
        exn "Folder is not empty and cannot be moved to a different drive"

    let cannotCopyNonEmptyFolder =
        exn "Folder is not empty and cannot be copied"

    let cannotDeleteNonEmptyFolder =
        exn "Folder is not empty and cannot be deleted"

open FakeFileSystemErrors

type FakeFileSystem(treeItems) =
    let mutable items = treeItems |> TreeItem.build
    let shortcuts = Dictionary<Path, string>()
    let mutable recycleBin = []
    let exnPaths = Dictionary<Path, (exn * bool) list>()
    let mutable callsToGetItems = 0

    let remove path =
        items <- items |> List.filter (fun item -> not (item.Path.IsWithin path))
        shortcuts.Remove(path) |> ignore

    let checkPathNotUsed path =
        if items |> List.exists (fun i -> i.Path = path) then
            Error (itemAlreadyExistsOnPath path)
        else
            Ok ()

    let checkExn isWrite path =
        match exnPaths.TryGetValue path with
        | true, exns ->
            let rec popWhere pred skipped lst =
                match lst with
                | [] -> (None, skipped)
                | x :: rest when pred x -> (Some x, skipped @ rest)
                | x :: rest -> popWhere pred (skipped @ [x]) rest
            let (exnItem, rest) = popWhere (fun (_, writeOnly) -> isWrite || not writeOnly) [] exns
            if rest |> List.isEmpty then
                exnPaths.Remove path |> ignore
            else
                exnPaths.[path] <- rest
            exnItem |> Option.map (fst >> Error) |? Ok ()
        | _ ->
            Ok ()

    let hasChildren path =
        items |> List.exists (fun i -> i.Path.Parent = path)

    member this.Items = items |> List.sortBy (fun i -> i.Path |> string |> String.toLower)

    member this.ItemsIn path =
        items |> List.filter (fun i -> i.Path.Parent = path)

    member this.Item path =
        items |> List.find (fun i -> i.Path = path)

    member this.RecycleBin = recycleBin

    member this.AddExnPath writeOnly e path =
        let exnItem = (e, writeOnly)
        match exnPaths.TryGetValue path with
        | true, exns ->
            exnPaths.[path] <- exns @ [exnItem]
        | _ ->
            exnPaths.Add(path, [exnItem])


    member this.CallsToGetItems = callsToGetItems

    interface IFileSystem

    interface IFileSystemReader with
        member this.GetItem path = this.GetItem path
        member this.GetItems path = this.GetItems path
        member this.GetFolders path = this.GetFolders path
        member this.IsEmpty path = this.IsEmpty path
        member this.GetShortcutTarget path = this.GetShortcutTarget path

    interface IFileSystemWriter with
        member this.Create itemType path = this.Create itemType path
        member this.CreateShortcut target path = this.CreateShortcut target path
        member this.Move itemType fromPath toPath = this.Move itemType fromPath toPath
        member this.Copy itemType fromPath toPath = this.Copy itemType fromPath toPath
        member this.Recycle itemType path = this.Recycle itemType path
        member this.Delete itemType path = this.Delete itemType path

    member this.GetItem path = result {
        do! checkExn false path
        return items |> List.tryFind (fun i -> i.Path = path)
    }

    member private this.AssertItem itemType path =
        this.GetItem path
        |> Result.bind (Result.ofOption (pathDoesNotExist path))
        |> Result.bind (fun item ->
            if item.Type <> itemType then
                Error (pathIsNotExpectedType path itemType item.Type)
            else
                Ok item
        )

    member this.GetItems path = result {
        callsToGetItems <- callsToGetItems + 1
        if path = Path.Root then
            return [Item.Basic (Path.Parse "C:").Value "C:" Drive]
        else
            do! checkExn false path
            return this.Items |> List.filter (fun i -> i.Path.Parent = path)
    }

    member this.GetFolders path =
        this.GetItems path |> Result.map (
            List.filter (fun i -> i.Type = Folder)
            >> List.sortBy (fun i -> i.Name.ToLower())
        )

    member this.IsEmpty path =
        match items |> List.tryFind (fun i -> i.Path = path) with
        | Some i when i.Type = Folder -> not (hasChildren path)
        | Some i when i.Type = File -> not (i.Size |> Option.exists (flip (>) 0L))
        | _ -> false

    member this.GetShortcutTarget path = result {
        do! checkExn false path
        match shortcuts.TryGetValue path with
        | true, p -> return p
        | _ -> return! Error notAShortcut
    }


    member this.Create itemType path = result {
        do! checkExn true path
        do! checkPathNotUsed path
        items <- Item.Basic path path.Name itemType :: items
    }

    member this.CreateShortcut target path = result {
        remove path
        do! this.Create File path
        shortcuts.Add(path, string target)
    }

    member private this.CheckDestParent (destPath: Path) = result {
        let! parent = this.GetItem destPath.Parent
        match parent with
        | None ->
            return! Error (destPathParentDoesNotExist destPath.Parent)
        | Some item when not (item.Type |> Seq.containedIn [Folder; Drive]) ->
            return! Error (destPathParentIsNotFolder destPath.Parent)
        | _ -> ()
    }

    member this.Move itemType fromPath toPath = result {
        let substitute oldItem newItem =
            items <- items |> List.map (fun i -> if i = oldItem then newItem else i)
        if String.equalsIgnoreCase (string fromPath) (string toPath) then
            let! item = this.AssertItem itemType fromPath
            substitute item { item with Path = toPath; Name = toPath.Name }
        else
            do! checkExn true fromPath
            let! item = this.AssertItem itemType fromPath
            if item.Type = Folder && fromPath.Base <> toPath.Base && hasChildren item.Path then
                return! Error cannotMoveNonEmptyFolderAcrossDrives
            do! this.CheckDestParent toPath
            do! checkExn true toPath
            if item.Type = Folder then
                do! checkPathNotUsed toPath
            else
                remove toPath
            substitute item { item with Path = toPath; Name = toPath.Name }

            match shortcuts.TryGetValue fromPath with
            | true, target ->
                shortcuts.Remove(fromPath) |> ignore
                shortcuts.Add(toPath, target)
            | _ -> ()

            if item.Type = Folder then
                for item in this.ItemsIn fromPath do
                    do! this.Move item.Type item.Path (toPath.Join item.Name)
    }

    member this.Copy itemType fromPath toPath = result {
        let! item = this.AssertItem itemType fromPath
        do! checkExn true toPath
        do! this.CheckDestParent toPath
        if item.Type = Folder && hasChildren item.Path then
            return! Error cannotCopyNonEmptyFolder
        if item.Type = Folder then
            do! checkPathNotUsed toPath
        else
            remove toPath
        let newItem = { item with Path = toPath; Name = toPath.Name }
        items <- newItem :: items
        match shortcuts.TryGetValue fromPath with
        | true, target -> shortcuts.Add(toPath, target)
        | _ -> ()
    }

    member this.Recycle itemType path = result {
        let! item = this.AssertItem itemType path
        do! checkExn true path
        recycleBin <- item :: recycleBin
        remove path
    }

    member this.Delete itemType path = result {
        let! item = this.AssertItem itemType path
        do! checkExn true path
        if item.Type = Folder && hasChildren item.Path then
            return! Error cannotDeleteNonEmptyFolder
        remove path
    }
