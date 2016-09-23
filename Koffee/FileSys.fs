module Koffee.FileSys

open System.IO
open Koffee

type PathService() =
    interface IPathService with
        override this.root = this.root
        override this.parent path = this.parent path
        override this.nodes path = this.nodes path

    member this.winPath (Path path) =
        path.TrimStart('/').Replace('/', '\\').Insert(1, ":")

    member this.unixPath (path: string) =
        "/" + path.Replace('\\', '/').Replace(":", "") |> Path

    member this.root = Path "/c/"

    member this.parent path =
        match path with
        | p when p = this.root -> p
        | p -> p |> this.winPath |> Path.GetDirectoryName |> this.unixPath

    member this.nodes path =
        let pathToNode nodeType path = {
            Name = Path.GetFileName path
            Path = this.unixPath path
            Type = nodeType
        }
        let wp = this.winPath path
        Seq.append
            (Directory.EnumerateDirectories wp |> Seq.map (pathToNode "Folder"))
            (Directory.EnumerateFiles wp |> Seq.map (pathToNode "File"))
        |> Seq.toList
