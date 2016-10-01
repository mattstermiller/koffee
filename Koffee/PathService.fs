namespace Koffee

open System
open System.IO
open Koffee

type PathService() =
    interface IPathService with
        override this.Root = this.Root
        override this.Parent path = this.Parent path
        override this.GetNodes path = this.GetNodes path
        override this.OpenFile path = this.OpenFile path
        override this.OpenExplorer path = this.OpenExplorer path

    member this.WinPath (Path path) =
        let p = path.TrimStart('/').Replace('/', '\\').Insert(1, ":")
        if p.EndsWith(":") then p + "\\" else p

    member this.UnixPath (path: string) =
        "/" + path.Replace('\\', '/').Replace(":", "") |> Path

    member this.Root = Path "/"

    member this.Parent path =
        match path with
        | p when p = this.Root -> p
        | Path p when p.Trim('/').Length <= 1 -> this.Root
        | p -> p |> this.WinPath |> Path.GetDirectoryName |> this.UnixPath

    member this.GetNodes path =
        if path = this.Root then
            DriveInfo.GetDrives() |> Seq.map this.DriveNode |> Seq.toList
        else
            let wp = this.WinPath path
            try
                let folders = Directory.EnumerateDirectories wp |> Seq.map this.FolderNode
                let files = Directory.EnumerateFiles wp |> Seq.map FileInfo |> Seq.map this.FileNode
                let nodes = Seq.append folders files |> Seq.toList
                if nodes.IsEmpty then
                    this.ErrorNode (Exception("Empty Folder")) (this.Parent path) |> List.singleton
                else nodes
            with | ex -> this.ErrorNode ex (this.Parent path) |> List.singleton

    member this.FileNode file = {
        Path = this.UnixPath file.FullName
        Name = file.Name
        Type = NodeType.File
        Modified = Some file.LastWriteTime
        Size = Some file.Length
    }

    member this.FolderNode path = {
        Path = this.UnixPath path
        Name = Path.GetFileName path
        Type = NodeType.Folder
        Modified = None
        Size = None
    }

    member this.DriveNode drive =
        let name = drive.Name.TrimEnd('\\')
        let driveType = drive.DriveType.ToString()
        let label = if drive.IsReady && drive.VolumeLabel <> "" then (sprintf " \"%s\"" drive.VolumeLabel) else ""
        {
            Path = drive.Name.ToLower() |> this.UnixPath
            Name = sprintf "%s  %s Drive%s" name driveType label
            Type = NodeType.Drive
            Modified = None
            Size = if drive.IsReady then Some drive.TotalSize else None
        }

    member this.ErrorNode ex path =
        let error = ex.Message.Split('\r', '\n').[0]
        {
            Path = path
            Name = sprintf "<%s>" error
            Type = NodeType.Error
            Modified = None
            Size = None
        }

    member this.OpenFile path =
        let winPath = this.WinPath path
        System.Diagnostics.Process.Start(winPath) |> ignore

    member this.OpenExplorer path =
        let winPath = this.WinPath path
        System.Diagnostics.Process.Start("explorer.exe", winPath) |> ignore
