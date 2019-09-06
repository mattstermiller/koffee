namespace Koffee

open System
open System.IO
open System.Text.RegularExpressions
open System.Management
open Microsoft.VisualBasic.FileIO
open Acadian.FSharp
open Koffee

type IFileSystemReader =
    abstract member GetItem: Path -> Result<Item option, exn>
    abstract member GetItems: Path -> Result<Item list, exn>
    abstract member GetFolders: Path -> Result<Item list, exn>
    abstract member IsEmpty: Path -> bool
    abstract member GetShortcutTarget: Path -> Result<string, exn>

type IFileSystemWriter =
    abstract member Create: ItemType -> Path -> Result<unit, exn>
    abstract member CreateShortcut: target: Path -> path: Path -> Result<unit, exn>
    abstract member Move: fromPath: Path -> toPath: Path -> Result<unit, exn>
    abstract member Copy: fromPath: Path -> toPath: Path -> Result<unit, exn>
    abstract member Recycle: Path -> Result<unit, exn>
    abstract member Delete: Path -> Result<unit, exn>

type FileSystem() =
    let wpath (path: Path) = path.Format Windows
    let toPath s = (Path.Parse s).Value

    let fileItem (file: FileInfo) =
        { Path = toPath file.FullName
          Name = file.Name
          Type = File
          Modified = Some file.LastWriteTime
          Size = Some file.Length
          IsHidden = file.Attributes.HasFlag FileAttributes.Hidden
        }

    let folderItem (dirInfo: DirectoryInfo) =
        { Path = toPath dirInfo.FullName
          Name = dirInfo.Name
          Type = Folder
          Modified = Some dirInfo.LastWriteTime
          Size = None
          IsHidden = dirInfo.Attributes.HasFlag FileAttributes.Hidden
        }

    let driveItem (drive: DriveInfo) =
        let name = drive.Name.TrimEnd('\\')
        let driveType =
            match drive.DriveType with
            | DriveType.Fixed -> "Hard"
            | dt -> dt.ToString()
        let label =
            if drive.IsReady && drive.VolumeLabel <> "" then
                (sprintf " \"%s\"" drive.VolumeLabel)
            else
                ""
        { Path = toPath drive.Name
          Name = sprintf "%s  %s Drive%s" name driveType label
          Type = Drive
          Modified = None
          Size = if drive.IsReady then Some drive.TotalSize else None
          IsHidden = false
        }

    let netShareItem path =
        { Item.Basic path path.Name NetShare with IsHidden = path.Name.EndsWith("$") }

    let getNetShares (serverPath: Path) =
        let server = serverPath.Name
        tryResult <| fun () ->
            use shares = new ManagementClass(sprintf @"\\%s\root\cimv2" server, "Win32_Share", new ObjectGetOptions())
            shares.GetInstances()
            |> Seq.cast<ManagementObject>
            |> Seq.map (fun s -> s.["Name"] :?> string)
            |> Seq.map (fun n -> serverPath.Join n |> netShareItem)

    let prepForOverwrite (file: FileInfo) =
        if file.Exists then
            if file.Attributes.HasFlag FileAttributes.System then
                raise <| UnauthorizedAccessException(sprintf "%s is a System file." file.Name)
            let flagsToClear = FileAttributes.ReadOnly ||| FileAttributes.Hidden
            if unbox<int> (file.Attributes &&& flagsToClear) <> 0 then
                file.Attributes <- file.Attributes &&& (~~~flagsToClear)

    let cannotActOnItemType action itemType =
        exn <| sprintf "Cannot %s item type %O" action itemType

    member private this.FileOrFolderAction path actionName action =
        match this.GetItem path with
        | Ok (Some item) when item.Type = File || item.Type = Folder -> action item.Type
        | Ok (Some item) -> Error <| cannotActOnItemType actionName item.Type
        | Ok None -> Error <| exn (sprintf "Path does not exist: %s" (wpath path))
        | Error e -> Error e


    interface IFileSystemReader with
        member this.GetItem path = this.GetItem path
        member this.GetItems path = this.GetItems false path
        member this.GetFolders path = this.GetItems true path
        member this.IsEmpty path = this.IsEmpty path
        member this.GetShortcutTarget path = this.GetShortcutTarget path

    member this.GetItem path =
        tryResult <| fun () ->
            let wp = wpath path
            let drive = lazy DriveInfo(wp)
            let dir = lazy DirectoryInfo(wp)
            let file = lazy FileInfo(wp)
            if path.IsNetHost then
                Some (Item.Basic path path.Name NetHost)
            else if path.Parent.IsNetHost && dir.Value.Exists then
                Some (path |> netShareItem)
            else if path.Drive = Some path then
                Some (drive.Value |> driveItem)
            else if dir.Value.Exists then
                Some (dir.Value |> folderItem)
            else if file.Value.Exists then
                Some (file.Value |> fileItem)
            else
                None

    member this.GetItems foldersOnly path =
        let items =
            if path = Path.Root then
                DriveInfo.GetDrives()
                |> Seq.map driveItem
                |> flip Seq.append [Item.Basic Path.Network "Network" Drive]
                |> Ok
            else if path.IsNetHost then
                getNetShares path
            else
                let dir = DirectoryInfo(wpath path)
                if dir.Exists then
                    tryResult <| fun () ->
                        let folders = dir.GetDirectories() |> Seq.map folderItem
                        if foldersOnly then
                            folders
                        else
                            Seq.append folders (dir.GetFiles() |> Seq.map fileItem)
                else
                    if path.Drive |> Option.exists (fun d -> not <| DriveInfo(wpath d).IsReady) then
                        Error <| exn "Drive is not ready"
                    else
                        Error <| exn (sprintf "Path does not exist: %s" (wpath path))
        items |> Result.map Seq.toList

    member this.IsEmpty path =
        let wp = wpath path
        try
            if Directory.Exists(wp) then
                Directory.EnumerateFiles(wp, "*", SearchOption.AllDirectories) |> Seq.isEmpty
            else
                FileInfo(wp).Length = 0L
        with _ -> false

    member this.GetShortcutTarget path =
        tryResult <| fun () ->
            LinkFile.getLinkTarget(wpath path)


    interface IFileSystemWriter with
        member this.Create itemType path = this.Create itemType path
        member this.CreateShortcut target path = this.CreateShortcut target path
        member this.Move fromPath toPath = this.Move fromPath toPath
        member this.Copy fromPath toPath = this.Copy fromPath toPath
        member this.Recycle path = this.Recycle path
        member this.Delete path = this.Delete path

    member this.Create itemType path =
        let wp = wpath path
        match itemType with
        | File -> tryResult (fun () -> File.Create(wp).Dispose())
        | Folder -> tryResult (fun () -> Directory.CreateDirectory(wp) |> ignore)
        | _ -> Error <| cannotActOnItemType "create" itemType

    member this.CreateShortcut target path =
        tryResult <| fun () ->
            LinkFile.saveLink (wpath target) (wpath path)

    member this.Move fromPath toPath =
        this.FileOrFolderAction fromPath "move" <| fun itemType -> result {
            let moveFile source dest =
                prepForOverwrite <| FileInfo dest
                FileSystem.MoveFile(source, dest, true)
            let rec moveDir sameVolume source dest =
                if not sameVolume || Directory.Exists dest then
                    // if dest directory exists, merge contents
                    let moveItem moveFunc sourceItem =
                        let destPath = Path.Combine(dest, Path.GetFileName(sourceItem))
                        moveFunc sourceItem destPath
                    Directory.EnumerateFiles(source) |> Seq.iter (moveItem moveFile)
                    Directory.EnumerateDirectories(source) |> Seq.iter (moveItem (moveDir sameVolume))
                    Directory.Delete(source)
                else
                    Directory.Move(source, dest)
            let source = wpath fromPath
            let dest = wpath toPath
            if String.equalsIgnoreCase source dest then
                let temp = sprintf "_rename_%s" fromPath.Name |> fromPath.Parent.Join
                do! this.Move fromPath temp
                do! this.Move temp toPath
            else
                return! tryResult <| fun () ->
                    if itemType = Folder then
                        let getVolume p = p |> Path.GetPathRoot |> String.trimEnd [|'\\'|]
                        let sameVolume = String.equalsIgnoreCase (getVolume source) (getVolume dest)
                        moveDir sameVolume source dest
                    else
                        moveFile source dest
        }

    member this.Copy fromPath toPath =
        this.FileOrFolderAction fromPath "copy" <| fun itemType ->
            let source = wpath fromPath
            let dest = wpath toPath
            let copyFile source dest =
                prepForOverwrite <| FileInfo dest
                File.Copy(source, dest, true)
            tryResult <| fun () ->
                if itemType = Folder then
                    let getDest sourcePath = Regex.Replace(sourcePath, "^" + (Regex.Escape source), dest)
                    // copy folder structure
                    Directory.CreateDirectory dest |> ignore
                    Directory.GetDirectories(source, "*", SearchOption.AllDirectories)
                    |> Seq.iter (fun dir -> getDest dir |> Directory.CreateDirectory |> ignore)
                    // copy files
                    Directory.GetFiles(source, "*", SearchOption.AllDirectories)
                    |> Seq.iter (fun file -> copyFile file (getDest file))
                else
                    copyFile source dest

    member this.Recycle path =
        this.FileOrFolderAction path "recycle" <| fun itemType -> result {
            let wp = wpath path
            let! driveSize =
                path.Drive
                |> Option.map (fun d -> DriveInfo(wpath d).TotalSize)
                |> Option.filter (flip (>) 0L)
                |> Result.ofOption (exn "This drive does not have a recycle bin.")
            let! item = this.GetItem path
            let! size =
                match item with
                | Some f when f.Type = File ->
                    Ok f.Size.Value
                | Some f when f.Type = Folder ->
                    tryResult <| fun () ->
                        DirectoryInfo(wp).EnumerateFiles("*", SearchOption.AllDirectories)
                        |> Seq.sumBy (fun fi -> fi.Length)
                | _ -> Error <| exn "Cannot recycle this item."
            let ratio = (double size) / (double driveSize)
            if ratio > 0.03 then
                return! Error <| exn "Item cannot fit in the recycle bin."
            else
                return! tryResult <| fun () ->
                    if itemType = Folder then
                        FileSystem.DeleteDirectory(wp, UIOption.OnlyErrorDialogs, RecycleOption.SendToRecycleBin)
                    else
                        FileSystem.DeleteFile(wp, UIOption.OnlyErrorDialogs, RecycleOption.SendToRecycleBin)
        }

    member this.Delete path =
        this.FileOrFolderAction path "delete" <| fun itemType ->
            let wp = wpath path
            tryResult <| fun () ->
                if itemType = Folder then
                    DirectoryInfo(wp).EnumerateFiles("*", SearchOption.AllDirectories)
                    |> Seq.iter prepForOverwrite
                    Directory.Delete(wp, true)
                else
                    prepForOverwrite <| FileInfo wp
                    File.Delete wp
