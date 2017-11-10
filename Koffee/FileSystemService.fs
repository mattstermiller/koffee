namespace Koffee

open System
open System.IO
open System.Text.RegularExpressions
open System.Diagnostics
open System.Management
open Microsoft.VisualBasic.FileIO
open Koffee
open Utility

type FileSystemService() =
    let wpath (path: Path) = path.Format Windows
    let toPath s = (Path.Parse s).Value

    member this.GetNode path =
        let wp = wpath path
        let dir = lazy DirectoryInfo(wp)
        let file = lazy FileInfo(wp)
        if path.IsNetHost then
            path |> this.NetHostNode |> Some
        else if dir.Value.Exists then
            dir.Value |> this.FolderNode |> Some
        else if file.Value.Exists then
            file.Value |> this.FileNode |> Some
        else
            None

    member this.GetNodes showHidden path =
        let error msg path =
            this.ErrorNode (Exception(msg)) path |> Seq.singleton
        let getNetShares server =
            use shares = new ManagementClass(sprintf @"\\%s\root\cimv2" server, "Win32_Share", new ObjectGetOptions())
            shares.GetInstances() |> Seq.cast<ManagementObject> |> Seq.map (fun s -> s.["Name"] :?> string)
        let allNodes =
            if path = Path.Root then
                DriveInfo.GetDrives() |> Seq.map this.DriveNode
            else if path.IsNetHost then
                getNetShares path.Name |> Seq.map (this.NetShareNode path)
            else
                let wp = wpath path
                if Directory.Exists wp then
                    let dir = DirectoryInfo(wp)
                    let folders = dir.GetDirectories() |> Seq.map this.FolderNode
                    let files = dir.GetFiles() |> Seq.map this.FileNode
                    Seq.append folders files
                else error "Path does not exist" Path.Root
        let nodes =
            allNodes
            |> Seq.filter (fun n -> not n.IsHidden || showHidden)
            |> Seq.toList
        if nodes.IsEmpty then
            { Path = path; Name = "<Empty Folder>"; Type = Empty
              Modified = None; Size = None; IsHidden = false; IsSearchMatch = false }
            |> List.singleton
        else nodes

    member this.IsEmpty path =
        let wp = wpath path
        if Directory.Exists(wp) then
            Directory.EnumerateFiles(wp, "*", SearchOption.AllDirectories) |> Seq.isEmpty
        else
            FileInfo(wp).Length = 0L

    member this.IsRecyclable path =
        let wp = wpath path
        let driveSize = path.Drive |> Option.map (fun d -> DriveInfo(d).TotalSize)
        let size =
            if driveSize.IsNone then
                None
            else if File.Exists(wp) then
                Some (FileInfo(wp).Length)
            else if Directory.Exists(wp) then
                Some (DirectoryInfo(wp).EnumerateFiles("*", SearchOption.AllDirectories)
                        |> Seq.sumBy (fun fi -> fi.Length))
            else
                None
        match size, driveSize with
            | Some s, Some ds when ds > 0L ->
                let ratio = (double s) / (double ds)
                ratio < 0.03
            | _ -> false

    member this.Create nodeType path =
        let wp = wpath path
        match nodeType with
            | File -> File.Create(wp).Dispose()
            | Folder -> Directory.CreateDirectory(wp) |> ignore
            | _ -> failwith (this.CannotActOnNodeType "create" nodeType)

    member this.Move currentPath newPath =
        let moveFile source dest =
            this.PrepForOverwrite <| FileInfo dest
            FileSystem.MoveFile(source, dest, true)
        let rec moveDir source dest =
            if Directory.Exists dest then
                // if dest directory exists, merge contents
                let moveItem moveFunc sourceItem =
                    let destPath = Path.Combine(dest, Path.GetFileName(sourceItem))
                    moveFunc sourceItem destPath
                Directory.EnumerateFiles(source) |> Seq.iter (moveItem moveFile)
                Directory.EnumerateDirectories(source) |> Seq.iter (moveItem moveDir)
                Directory.Delete(source)
            else
                Directory.Move(source, dest)
        let source = wpath currentPath
        let dest = wpath newPath
        if Str.equalsIgnoreCase source dest then
            let temp = sprintf "_rename_%s" currentPath.Name |> currentPath.Parent.Join
            this.Move currentPath temp
            this.Move temp newPath
        else
            if Directory.Exists source then
                moveDir source dest
            else
                moveFile source dest

    member this.Copy currentPath newPath =
        let source = wpath currentPath
        let dest = wpath newPath
        let copyFile source dest =
            this.PrepForOverwrite <| FileInfo dest
            File.Copy(source, dest, true)
        if Directory.Exists source then
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
        let wp = wpath path
        if Directory.Exists wp then
            FileSystem.DeleteDirectory(wp, UIOption.OnlyErrorDialogs, RecycleOption.SendToRecycleBin)
        else
            FileSystem.DeleteFile(wp, UIOption.OnlyErrorDialogs, RecycleOption.SendToRecycleBin)

    member this.Delete path =
        let wp = wpath path
        let dir = DirectoryInfo(wp)
        if dir.Exists then
            dir.EnumerateFiles("*", SearchOption.AllDirectories)
                |> Seq.iter this.PrepForOverwrite
            Directory.Delete(wp, true)
        else
            this.PrepForOverwrite <| FileInfo wp
            File.Delete wp

    member this.OpenFile path =
        ProcessStartInfo(wpath path, WorkingDirectory = wpath path.Parent)
        |> Process.Start |> ignore

    member this.OpenExplorer node =
        match node.Type with
        | File | Folder when node.Path <> Path.Root ->
            Process.Start("explorer.exe", sprintf "/select,\"%s\"" (wpath node.Path)) |> ignore
        | Drive | Empty ->
            Process.Start("explorer.exe", sprintf "\"%s\"" (wpath node.Path)) |> ignore
        | _ ->
            Process.Start("explorer.exe") |> ignore

    member this.LaunchApp exePath workingPath args =
        ProcessStartInfo(exePath, args, WorkingDirectory = wpath workingPath)
        |> Process.Start |> ignore


    member private this.PrepForOverwrite file =
        if file.Exists then
            if file.Attributes.HasFlag FileAttributes.System then
                raise <| UnauthorizedAccessException(sprintf "%s is a System file." file.Name)
            let flagsToClear = FileAttributes.ReadOnly ||| FileAttributes.Hidden
            if unbox<int> (file.Attributes &&& flagsToClear) <> 0 then
                file.Attributes <- file.Attributes &&& (~~~flagsToClear)

    member private this.FileNode file = {
        Path = toPath file.FullName
        Name = file.Name
        Type = File
        Modified = Some file.LastWriteTime
        Size = Some file.Length
        IsHidden = file.Attributes.HasFlag FileAttributes.Hidden
        IsSearchMatch = false
    }

    member private this.FolderNode dirInfo = {
        Path = toPath dirInfo.FullName
        Name = dirInfo.Name
        Type = Folder
        Modified = None
        Size = None
        IsHidden = dirInfo.Attributes.HasFlag FileAttributes.Hidden
        IsSearchMatch = false
    }

    member private this.DriveNode drive =
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
          IsSearchMatch = false
        }

    member private this.NetHostNode path = {
        Path = path
        Name = path.Name
        Type = Folder
        Modified = None
        Size = None
        IsHidden = false
        IsSearchMatch = false
    }

    member private this.NetShareNode serverPath name = {
        Path = serverPath.Join name
        Name = name
        Type = Folder
        Modified = None
        Size = None
        IsHidden = name.EndsWith("$")
        IsSearchMatch = false
    }

    member private this.ErrorNode ex path =
        let error = ex.Message.Split('\r', '\n').[0]
        { Path = path
          Name = sprintf "<%s>" error
          Type = ErrorNode
          Modified = None
          Size = None
          IsHidden = false
          IsSearchMatch = false
        }

    member private this.CannotActOnNodeType action nodeType =
        sprintf "Cannot %s node type %A" action nodeType
