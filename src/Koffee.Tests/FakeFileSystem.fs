namespace Koffee

open System.Collections.Generic
open Acadian.FSharp

type TreeItem =
    | TreeFile of name: string * transform: (Item -> Item)
    | TreeFolder of name: string * TreeItem list
    | TreeDrive of driveLetter: char * size: int64 option * TreeItem list
    | TreeNetHost of string * TreeItem list
    | TreeNetwork of TreeItem list

[<AutoOpen>]
module FakeFileSystem =
    let createPath pathStr =
        Path.Parse pathStr |> Option.defaultWith (fun () -> failwithf "Invalid path: %s" pathStr)

    let createFile pathStr =
        let path = createPath pathStr
        Item.Basic path path.Name File

    let createFolder pathStr =
        let path = createPath pathStr
        let name = if path.Name |> String.isNotEmpty then path.Name else string path |> String.substring 0 1
        Item.Basic path name Folder

    let createDrive (driveLetter: char) =
        let name = driveLetter |> string |> String.toUpper |> fun l -> l + ":"
        Item.Basic (createPath name) (name + " Hard Drive") Drive

    let file name = TreeFile (name, id)
    let fileWith transform name = TreeFile (name, transform)
    let folder name items = TreeFolder (name, items)
    let drive name items = TreeDrive (name, None, items)
    let driveWithSize name size items = TreeDrive (name, Some size, items)
    let netHost name items = TreeNetHost (name, items)
    let network items = TreeNetwork items

    let size value (item: Item) = { item with Size = Some value }
    let modifiedOpt opt (item: Item) = { item with Modified = opt }
    let modified value item = modifiedOpt (Some value) item
    let hide value item = { item with IsHidden = value }

    let sortByPath items =
        items |> List.sortBy (fun i -> i.Path |> string |> String.toLower)

    type TreeItem with
        static member build items =
            let rec buildIter (path: Path) items =
                items |> List.collect (fun item ->
                    match item with
                    | TreeFile (name, transform) ->
                        Item.Basic (path.Join name) name File |> transform |> List.singleton
                    | TreeFolder (name, items) ->
                        let folder = Item.Basic (path.Join name) name Folder
                        folder :: buildIter folder.Path items
                    | TreeNetHost _ when path <> Path.Network ->
                        failwithf "Tree net host is invalid because it is not in Network: %O" item
                    | TreeNetHost (name, items) ->
                        let host = Item.Basic (path.Join name) name NetHost
                        host :: buildIter host.Path items
                    | TreeDrive _ | TreeNetwork _ when path <> Path.Root ->
                        failwithf "Tree item is invalid because it is not at root level: %O" item
                    | TreeDrive (letter, size, items) ->
                        let drive = { createDrive letter with Size = size }
                        drive :: buildIter drive.Path items
                    | TreeNetwork items ->
                        if not (items |> List.forall (function TreeNetHost _ -> true | _ -> false)) then
                            failwithf "Network cannot contain items that are not NetHosts"
                        let net = Item.Basic Path.Network "Network" Drive
                        net :: buildIter net.Path items
                )
            if items |> List.forall (function TreeDrive _ | TreeNetwork _ -> true | _ -> false) then
                buildIter Path.Root items
            else
                let drive = createDrive 'c'
                drive :: buildIter drive.Path items

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

    let cannotRecycleItemOnDriveWithNoSize =
        exn "Item is on drive with no Recycle Bin"

    let cannotRecycleItemThatDoesNotFit (totalSize: int64) =
        exn (sprintf "Item size of %O is too large to fit in the Recycle Bin" totalSize)

    let cannotDeleteNonEmptyFolder =
        exn "Folder is not empty and cannot be deleted"

open FakeFileSystemErrors

type FakeFileSystem(treeItems) =
    let mutable items = treeItems |> TreeItem.build
    let shortcuts = Dictionary<Path, string>()
    let mutable recycleBin = []
    let exnPaths = Dictionary<Path, (exn * bool) list>()
    let mutable callsToGetItems = 0
    let mutable tokenToCancelAfterWrites: (CancelToken * int) option = None

    let tryFindItem path =
        items |> List.tryFind (fun i -> i.Path = path)

    let remove path =
        items <- items |> List.filter (fun item -> not (item.Path.IsWithin path))
        shortcuts.Remove(path) |> ignore

    let checkPathNotUsed path =
        if items |> List.exists (fun i -> i.Path = path) then
            Error (itemAlreadyExistsOnPath path)
        else
            Ok ()

    let checkExn isWrite path =
        match exnPaths.TryGetValueOption path with
        | Some exns ->
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
        | None ->
            Ok ()

    let checkCancelToken () =
        tokenToCancelAfterWrites |> Option.iter (fun (token, count) ->
            if count > 1 then
                tokenToCancelAfterWrites <- Some (token, count-1)
            else
                token.Cancel()
                tokenToCancelAfterWrites <- None
        )

    let hasChildren path =
        items |> List.exists (fun i -> i.Path.Parent = path)

    let getDriveSize (path: Path) =
        path.Drive
        |> Option.filter ((<>) path)
        |> Option.bind tryFindItem
        |> Option.bind (fun drive -> drive.Size)

    member this.Items = items |> sortByPath

    member this.ItemsIn path =
        items |> List.filter (fun i -> i.Path.Parent = path)

    member this.Item path =
        items |> List.find (fun i -> i.Path = path)

    member this.RecycleBin = recycleBin |> List.rev

    member this.AddExnPath writeOnly e path =
        let exnItem = (e, writeOnly)
        match exnPaths.TryGetValueOption path with
        | Some exns ->
            exnPaths.[path] <- exns @ [exnItem]
        | None ->
            exnPaths.Add(path, [exnItem])

    member this.CancelAfterWriteCount writes cancelToken =
        tokenToCancelAfterWrites <- Some (cancelToken, writes)


    member this.CallsToGetItems = callsToGetItems

    interface IFileSystem

    interface IFileSystemReader with
        member this.GetItem path = this.GetItem path
        member this.GetItems path = this.GetItems path
        member this.GetFolders path = this.GetFolders path
        member this.GetShortcutTarget path = this.GetShortcutTarget path
        member this.IsEmpty path = this.IsEmpty path
        member this.IsPathRecyclable path = this.IsPathRecyclable path

    interface IFileSystemWriter with
        member this.Create itemType path = this.Create itemType path
        member this.CreateShortcut target path = this.CreateShortcut target path
        member this.Move itemType fromPath toPath = this.Move itemType fromPath toPath
        member this.Copy itemType fromPath toPath = this.Copy itemType fromPath toPath
        member this.CheckRecyclable totalSize path = this.CheckRecyclable totalSize path
        member this.Recycle itemType path = this.Recycle itemType path
        member this.Delete itemType path = this.Delete itemType path

    member this.GetItem path = result {
        do! checkExn false path
        return tryFindItem path
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
        do! checkExn false path
        return this.Items |> List.filter (fun i -> i.Path.Parent = path)
    }

    member this.GetFolders path =
        this.GetItems path |> Result.map (
            List.filter (fun i -> i.Type = Folder)
            >> List.sortBy (fun i -> i.Name.ToLower())
        )

    member this.GetShortcutTarget path = result {
        do! checkExn false path
        return! shortcuts.TryGetValueOption path |> Result.ofOption notAShortcut
    }

    member this.IsEmpty path =
        match tryFindItem path with
        | Some i when i.Type = Folder -> not (hasChildren path)
        | Some i when i.Type = File -> not (i.Size |> Option.exists (fun size -> size > 0L))
        | _ -> false

    member this.IsPathRecyclable (path: Path) =
        getDriveSize path
        |> Option.exists (fun size -> size > 0L)


    member this.Create itemType path = result {
        do! checkExn true path
        let createItem () =
            items <- Item.Basic path path.Name itemType :: items
        match tryFindItem path with
        | Some existing when existing.Type <> itemType ->
            return! Error (itemAlreadyExistsOnPath path)
        | Some _ when itemType = Folder ->
            ()
        | Some _ ->
            remove path
            createItem ()
        | None ->
            createItem ()
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

            shortcuts.TryGetValueOption fromPath |> Option.iter (fun target ->
                shortcuts.Remove(fromPath) |> ignore
                shortcuts.Add(toPath, target)
            )

            if item.Type = Folder then
                for item in this.ItemsIn fromPath do
                    do! this.Move item.Type item.Path (toPath.Join item.Name)
        checkCancelToken ()
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
        shortcuts.TryGetValueOption fromPath |> Option.iter (fun target ->
            shortcuts.Add(toPath, target)
        )
        checkCancelToken ()
    }

    member this.CheckRecyclable (totalSize: int64) path =
        getDriveSize path |> Result.ofOption cannotRecycleItemOnDriveWithNoSize
        |> Result.bind (fun driveSize ->
            let ratio = float totalSize / float driveSize
            if ratio > 0.03
            then Error (cannotRecycleItemThatDoesNotFit totalSize)
            else Ok ()
        )

    member this.Recycle itemType path = result {
        let! item = this.AssertItem itemType path
        do! checkExn true path
        recycleBin <- item :: recycleBin
        remove path
        checkCancelToken ()
    }

    member this.Delete itemType path = result {
        let! item = this.AssertItem itemType path
        do! checkExn true path
        if item.Type = Folder && hasChildren item.Path then
            return! Error cannotDeleteNonEmptyFolder
        remove path
        checkCancelToken ()
    }
