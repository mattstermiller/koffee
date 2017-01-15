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
    abstract IsEmpty: Node -> bool
    abstract IsRecyclable: Node -> bool
    abstract Create: NodeType -> Path -> unit
    abstract Rename: NodeType -> Path -> string -> unit
    abstract Delete: Node -> unit
    abstract DeletePermanently: Node -> unit
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
        override this.Rename nodeType path newName = this.Rename nodeType path newName
        override this.Delete node = this.Delete node
        override this.DeletePermanently node = this.DeletePermanently node
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
            let winPath = this.ToRawPath path
            let folders = Directory.EnumerateDirectories winPath |> Seq.map this.FolderNode
            let files = DirectoryInfo(winPath).GetFiles() |> Seq.map this.FileNode
            let nodes = Seq.append folders files |> Seq.toList
            if nodes.IsEmpty then
                this.ErrorNode (Exception("Empty Folder")) (this.Parent path) |> List.singleton
            else nodes

    member this.IsEmpty (node: Node) =
        let winPath = this.ToRawPath node.Path
        match node.Type with
            | File -> FileInfo(winPath).Length = 0L
            | Folder -> Directory.EnumerateFiles(winPath) |> Seq.isEmpty
            | _ -> false

    member this.IsRecyclable (node: Node) =
        let winPath = this.ToRawPath node.Path
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
            | _ -> failwith (sprintf "Cannot create node type %A" nodeType)

    member this.Rename nodeType path newName =
        let rawPath = this.ToRawPath path
        let newPath = Path.Combine(Path.GetDirectoryName(rawPath), newName)
        match nodeType with
            | File -> File.Move(rawPath, newPath)
            | Folder -> Directory.Move(rawPath, newPath)
            | _ -> failwith (sprintf "Cannot rename node type %A" nodeType)

    member this.Delete node =
        let rawPath = this.ToRawPath node.Path
        match node.Type with
            | File -> FileSystem.DeleteFile(rawPath, UIOption.OnlyErrorDialogs, RecycleOption.SendToRecycleBin)
            | Folder -> FileSystem.DeleteDirectory(rawPath, UIOption.OnlyErrorDialogs, RecycleOption.SendToRecycleBin)
            | _ -> failwith (sprintf "Cannot delete node type %A" node.Type)

    member this.DeletePermanently node =
        let rawPath = this.ToRawPath node.Path
        match node.Type with
            | File -> File.Delete rawPath
            | Folder -> Directory.Delete rawPath
            | _ -> failwith (sprintf "Cannot delete node type %A" node.Type)


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
