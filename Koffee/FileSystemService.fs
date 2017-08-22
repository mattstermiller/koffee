namespace Koffee

open System
open System.IO
open System.Text.RegularExpressions
open System.Diagnostics
open Microsoft.VisualBasic.FileIO
open Koffee

type IFileSystemService =
    abstract GetNode: Path -> Node option
    abstract GetNodes: Path -> showHidden: bool-> Node list
    abstract Exists: Path -> bool
    abstract IsEmpty: Path -> bool
    abstract IsRecyclable: Path -> bool
    abstract Create: NodeType -> Path -> unit
    abstract Move: currentPath: Path -> newPath: Path -> unit
    abstract Copy: currentPath: Path -> newPath: Path -> unit
    abstract Recycle: Path -> unit
    abstract Delete: Path -> unit
    abstract OpenFile: Path -> unit
    abstract OpenExplorer: Path -> unit
    abstract OpenCommandLine: Path -> unit
    abstract OpenWith: exePath: string -> itemPath: Path -> unit

type FileSystemService() =
    let wpath (path: Path) = path.Format Windows
    let toPath s = (Path.Parse s).Value

    interface IFileSystemService with
        override this.GetNode path = this.GetNode path
        override this.GetNodes path showHidden = this.GetNodes path showHidden
        override this.Exists path = this.Exists path
        override this.IsEmpty node = this.IsEmpty node
        override this.IsRecyclable node = this.IsRecyclable node
        override this.Create nodeType path = this.Create nodeType path
        override this.Move currentPath newPath = this.Move currentPath newPath
        override this.Copy currentPath newPath = this.Copy currentPath newPath
        override this.Recycle node = this.Recycle node
        override this.Delete node = this.Delete node
        override this.OpenFile path = this.OpenFile path
        override this.OpenExplorer path = this.OpenExplorer path
        override this.OpenCommandLine path = this.OpenCommandLine path
        override this.OpenWith exePath itemPath = this.OpenWith exePath itemPath

    member this.GetNode path =
        let wp = wpath path
        if Directory.Exists wp then
            DirectoryInfo(wp) |> this.FolderNode |> Some
        else if File.Exists wp then
            FileInfo(wp) |> this.FileNode |> Some
        else
            None

    member this.GetNodes path showHidden =
        let error msg parent =
            this.ErrorNode (Exception(msg)) parent |> List.singleton
        if path = Path.Root then
            DriveInfo.GetDrives() |> Seq.map this.DriveNode |> Seq.toList
        else
            let wp = wpath path
            if Directory.Exists wp then
                let dir = DirectoryInfo(wp)
                let folders = dir.GetDirectories() |> Seq.map this.FolderNode
                let files = dir.GetFiles() |> Seq.map this.FileNode
                let nodes =
                    Seq.append folders files
                    |> Seq.filter (fun n -> not n.IsHidden || showHidden)
                    |> Seq.toList
                if nodes.IsEmpty then
                    error "Empty Folder" path.Parent
                else nodes
            else error "Path does not exist" Path.Root

    member this.Exists path =
        let wp = wpath path
        File.Exists wp || Directory.Exists wp

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
        Process.Start(wpath path) |> ignore

    member this.OpenExplorer path =
        if path <> Path.Root then
            Process.Start("explorer.exe", sprintf "/select,\"%s\"" (wpath path)) |> ignore

    member this.OpenCommandLine path =
        if path <> Path.Root then
            Process.Start("cmd.exe", sprintf "/k pushd \"%s\"" (wpath path)) |> ignore

    member this.OpenWith exePath itemPath =
        Process.Start(exePath, sprintf "\"%s\"" (wpath itemPath)) |> ignore


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
        Type = NodeType.File
        Modified = Some file.LastWriteTime
        Size = Some file.Length
        IsHidden = file.Attributes.HasFlag FileAttributes.Hidden
        IsSearchMatch = false
    }

    member private this.FolderNode dirInfo = {
        Path = toPath dirInfo.FullName
        Name = dirInfo.Name
        Type = NodeType.Folder
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
        {
            Path = toPath drive.Name
            Name = sprintf "%s  %s Drive%s" name driveType label
            Type = NodeType.Drive
            Modified = None
            Size = if drive.IsReady then Some drive.TotalSize else None
            IsHidden = false
            IsSearchMatch = false
        }

    member private this.ErrorNode ex path =
        let error = ex.Message.Split('\r', '\n').[0]
        {
            Path = path
            Name = sprintf "<%s>" error
            Type = NodeType.Error
            Modified = None
            Size = None
            IsHidden = false
            IsSearchMatch = false
        }

    member private this.CannotActOnNodeType action nodeType =
        sprintf "Cannot %s node type %A" action nodeType
