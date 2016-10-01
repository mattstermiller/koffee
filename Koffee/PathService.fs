namespace Koffee

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

    member this.Root = Path "/c/"

    member this.Parent path =
        match path with
        | p when p = this.Root -> p
        | p -> p |> this.WinPath |> Path.GetDirectoryName |> this.UnixPath

    member this.GetNodes path =
        let wp = this.WinPath path
        let folders = Directory.EnumerateDirectories wp |> Seq.map this.FolderNode
        let files = Directory.EnumerateFiles wp |> Seq.map FileInfo |> Seq.map this.FileNode
        Seq.append folders files |> Seq.toList

    member this.FolderNode path = {
        Path = this.UnixPath path
        Name = Path.GetFileName path
        Type = NodeType.Folder
        Modified = None
        Size = None
    }

    member this.FileNode (file: FileInfo) = {
        Path = this.UnixPath file.FullName
        Name = file.Name
        Type = NodeType.File
        Modified = Some file.LastWriteTime
        Size = Some file.Length
    }

    member this.OpenFile path =
        let winPath = this.WinPath path
        System.Diagnostics.Process.Start(winPath) |> ignore

    member this.OpenExplorer path =
        let winPath = this.WinPath path
        System.Diagnostics.Process.Start("explorer.exe", winPath) |> ignore
