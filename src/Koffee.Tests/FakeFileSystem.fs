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

    let pathAlreadyUsed (path: Path) =
        exn ("Path already used: " + string path)

    let notAShortcut =
        exn "Not a shortcut"

    let destPathParentDoesNotExist (path: Path) =
        exn ("Destination folder does not exist: " + string path)

    let destPathParentIsNotFolder (path: Path) =
        exn ("Destination path is not a folder: " + string path)

    let destPathIsFolder (path: Path) =
        exn ("Folder exists at destination path: " + string path)

open FakeFileSystemErrors

type FakeFileSystem(treeItems) =
    let mutable items = treeItems |> TreeItem.build
    let shortcuts = Dictionary<Path, string>()
    let mutable recycleBin = []
    let exnPaths = Dictionary<Path, (exn * bool) list>()
    let mutable callsToGetItems = 0

    let remove path =
        let itemWithinPath (path: Path) item =
            item.Path.FormatFolder Windows |> String.startsWithIgnoreCase (path.FormatFolder Windows)
        items <- items |> List.filter (not << itemWithinPath path)
        shortcuts.Remove(path) |> ignore

    let checkPathNotUsed path =
        if items |> List.exists (fun i -> i.Path = path) then
            Error (pathAlreadyUsed path)
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
        member this.Move fromPath toPath = this.Move fromPath toPath
        member this.Copy fromPath toPath = this.Copy fromPath toPath
        member this.Recycle path = this.Recycle path
        member this.Delete path = this.Delete path

    member this.GetItem path = result {
        do! checkExn false path
        return items |> List.tryFind (fun i -> i.Path = path)
    }

    member private this.AssertItem path = result {
        let! item = this.GetItem path
        return! item |> Result.ofOption (pathDoesNotExist path)
    }

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
        | Some i when i.Type = Folder -> not (items |> List.exists (fun i -> i.Path.Parent = path))
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

    member this.Move fromPath toPath = result {
        if String.equalsIgnoreCase (string fromPath) (string toPath) then
            let! item = this.AssertItem fromPath
            let newItem = { item with Path = toPath; Name = toPath.Name }
            items <- items |> List.map (fun i -> if i.Path = fromPath then newItem else i)
        else
            do! checkExn true fromPath
            do! this.Copy fromPath toPath
            let! _ = this.AssertItem fromPath
            remove fromPath
    }

    member this.Copy fromPath toPath = result {
        let! item = this.AssertItem fromPath
        do! checkExn true toPath
        let! parent = this.GetItem toPath.Parent
        do!
            match parent with
            | None ->
                Error (destPathParentDoesNotExist toPath.Parent)
            | Some item when not (item.Type |> Seq.containedIn [Folder; Drive]) ->
                Error (destPathParentIsNotFolder toPath.Parent)
            | _ -> Ok ()
        remove toPath
        let newItem = { item with Path = toPath; Name = toPath.Name }
        items <- newItem :: items
        match shortcuts.TryGetValue fromPath with
        | true, target -> shortcuts.Add(toPath, target)
        | _ -> ()
        match item.Type with
        | Folder ->
            let! folderItems = this.GetItems fromPath
            for item in folderItems do
                do! this.Copy item.Path (toPath.Join item.Name)
        | _ -> ()
    }

    member this.Recycle path = result {
        let! item = this.AssertItem path
        recycleBin <- item :: recycleBin
        return! this.Delete path
    }

    member this.Delete path = result {
        let! _ = this.AssertItem path
        do! checkExn true path
        remove path
    }
