namespace Koffee

open System
open System.IO
open System.Text.RegularExpressions
open System.Diagnostics
open Microsoft.VisualBasic.FileIO
open Koffee

type PathFormat =
    | Windows
    | Unix
    override this.ToString() = sprintf "%A" this

type IFileSystemService =
    abstract Format: PathFormat with get, set
    abstract Root: Path with get
    abstract Normalize: Path -> Path
    abstract Parent: Path -> Path
    abstract JoinPath: Path -> string -> Path
    abstract GetNodes: Path -> Node list
    abstract IsEmpty: Path -> bool
    abstract IsRecyclable: Path -> bool
    abstract Create: NodeType -> Path -> unit
    abstract Move: currentPath: Path -> newPath: Path -> unit
    abstract Recycle: Path -> unit
    abstract Delete: Path -> unit
    abstract OpenFile: Path -> unit
    abstract OpenExplorer: Path -> unit

type FileSystemService() =
    let (|WinPath|_|) path =
        match path with
        | (Path p) when Regex.IsMatch(p, @"^[a-z]:", RegexOptions.IgnoreCase) -> Some p
        | _ -> None

    let (|UnixPath|_|) path =
        match path with
        | (Path p) when Regex.IsMatch(p, @"^/[a-z]", RegexOptions.IgnoreCase) -> Some p
        | _ -> None

    interface IFileSystemService with
        member this.Format
            with get () = this.PathFormat
            and set (v: PathFormat) : unit = this.PathFormat <- v
        override this.Root = this.Root
        override this.Parent path = this.Parent path
        override this.JoinPath path name = this.JoinPath path name
        override this.Normalize path = this.Normalize path
        override this.GetNodes path = this.GetNodes path
        override this.IsEmpty node = this.IsEmpty node
        override this.IsRecyclable node = this.IsRecyclable node
        override this.Create nodeType path = this.Create nodeType path
        override this.Move currentPath newPath = this.Move currentPath newPath
        override this.Recycle node = this.Recycle node
        override this.Delete node = this.Delete node
        override this.OpenFile path = this.OpenFile path
        override this.OpenExplorer path = this.OpenExplorer path

    member val PathFormat = Windows with get, set

    member this.ToWinPath (Path path) =
        let p = path.TrimStart('/').Replace('/', '\\').Insert(1, ":")
        if p.EndsWith(":") then p + "\\" else p

    member this.ToUnixPath (path: string) =
        "/" + path.Replace('\\', '/').Replace(":", "") |> Path

    member this.ToRawPath path =
        match this.PathFormat with
        | Unix -> this.ToWinPath path
        | Windows -> path.Value

    member this.ToFormattedPath path =
        match this.PathFormat with
        | Unix -> this.ToUnixPath path
        | Windows -> Path path

    member this.Root =
        match this.PathFormat with
        | Unix -> Path "/"
        | Windows -> Path "Drives"

    member this.Normalize path =
        match this.PathFormat with
        | Unix ->
            let converted =
                match path with
                | WinPath p -> (this.ToUnixPath p).Value
                | (Path p) -> p.Replace('\\', '/')
            match converted.Trim('/') |> List.ofSeq with
            | drive :: rest ->
                let pathCore = Char.ToLower(drive) :: rest |> Array.ofList |> String
                "/" + pathCore |> Path
            | _ -> this.Root
        | Windows -> 
            let converted =
                match path with
                | UnixPath p -> this.ToWinPath (Path p)
                | (Path p) -> p.Replace('/', '\\')
            match converted.Trim('\\') |> List.ofSeq with
            | drive :: rest ->
                let newRest =
                    if rest.Length <= 2 then [':'; '\\']
                    else rest
                Char.ToUpper(drive) :: newRest |> Array.ofList |> String |> Path
            | _ -> this.Root

    member this.Parent path =
        match path with
        | p when p = this.Root -> p
        | Path p when p.Trim('/', '\\').Length <= 2 -> this.Root
        | p -> p |> this.ToRawPath |> Path.GetDirectoryName |> this.ToFormattedPath

    member this.JoinPath (path: Path) name =
        let trimmed = path.Value.Trim('/', '\\')
        let join =
            match this.PathFormat with
            | Unix -> '/'
            | Windows -> '\\'
        sprintf "%s%O%s" trimmed join name |> Path

    member this.GetNodes path =
        if path = this.Root then
            DriveInfo.GetDrives() |> Seq.map this.DriveNode |> Seq.toList
        else
            let rawPath = this.ToRawPath path
            if Directory.Exists rawPath then
                let folders = Directory.EnumerateDirectories rawPath |> Seq.map this.FolderNode
                let files = DirectoryInfo(rawPath).GetFiles() |> Seq.map this.FileNode
                let nodes = Seq.append folders files |> Seq.toList
                if nodes.IsEmpty then
                    this.ErrorNode (Exception("Empty Folder")) (this.Parent path) |> List.singleton
                else nodes
            else
                this.ErrorNode (Exception("Path does not exist")) this.Root |> List.singleton

    member this.IsEmpty path =
        let winPath = this.ToRawPath path
        if Directory.Exists(winPath) then
            Directory.EnumerateFiles(winPath, "*", SearchOption.AllDirectories) |> Seq.isEmpty
        else
            FileInfo(winPath).Length = 0L

    member this.IsRecyclable path =
        let winPath = this.ToRawPath path
        let driveSize =
            match winPath.[0] with
            | l when Char.IsLetter l -> Some (DriveInfo(string l).TotalSize)
            | _ -> None
        let size =
            if driveSize.IsNone then
                None
            else if File.Exists(winPath) then
                Some (FileInfo(winPath).Length)
            else if Directory.Exists(winPath) then
                Some (DirectoryInfo(winPath).EnumerateFiles("*", SearchOption.AllDirectories)
                        |> Seq.sumBy (fun fi -> fi.Length))
            else
                None
        match size, driveSize with
            | Some s, Some ds when ds > 0L ->
                let ratio = (double s) / (double ds)
                ratio < 0.03
            | _ -> false

    member this.Create nodeType path =
        let winPath = this.ToRawPath path
        match nodeType with
            | File -> File.Create(winPath).Dispose()
            | Folder -> Directory.CreateDirectory(winPath) |> ignore
            | _ -> failwith (this.CannotActOnNodeType "create" nodeType)

    member this.Move currentPath newPath =
        let source = this.ToRawPath currentPath
        let dest = this.ToRawPath newPath
        if Directory.Exists source then
            Directory.Move(source, dest)
        else
            File.Move(source, dest)

    member this.Recycle path =
        let rawPath = this.ToRawPath path
        if Directory.Exists rawPath then
            FileSystem.DeleteDirectory(rawPath, UIOption.OnlyErrorDialogs, RecycleOption.SendToRecycleBin)
        else
            FileSystem.DeleteFile(rawPath, UIOption.OnlyErrorDialogs, RecycleOption.SendToRecycleBin)

    member this.Delete path =
        let rawPath = this.ToRawPath path
        if Directory.Exists rawPath then
            Directory.Delete rawPath
        else
            File.Delete rawPath


    member this.OpenFile path =
        let winPath = this.ToRawPath path
        Process.Start(winPath) |> ignore

    member this.OpenExplorer path =
        if path <> this.Root then
            let winPath = this.ToRawPath path
            Process.Start("explorer.exe", String.Format("/select,\"{0}\"", winPath)) |> ignore


    member private this.FileNode file = {
        Path = this.ToFormattedPath file.FullName
        Name = file.Name
        Type = NodeType.File
        Modified = Some file.LastWriteTime
        Size = Some file.Length
    }

    member private this.FolderNode path = {
        Path = this.ToFormattedPath path
        Name = Path.GetFileName path
        Type = NodeType.Folder
        Modified = None
        Size = None
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
            Path = drive.Name |> this.ToFormattedPath
            Name = sprintf "%s  %s Drive%s" name driveType label
            Type = NodeType.Drive
            Modified = None
            Size = if drive.IsReady then Some drive.TotalSize else None
        }

    member private this.ErrorNode ex path =
        let error = ex.Message.Split('\r', '\n').[0]
        {
            Path = path
            Name = sprintf "<%s>" error
            Type = NodeType.Error
            Modified = None
            Size = None
        }

    member private this.CannotActOnNodeType action nodeType =
        sprintf "Cannot %s node type %A" action nodeType
