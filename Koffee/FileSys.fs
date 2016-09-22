module Koffee.FileSys

open System.IO

type PathService() =
    interface IPathService with
        override x.root = @"C:\"

        override x.parent path = Path.GetDirectoryName path

        override x.nodes path =
            let pathToNode nodeType path = {
                Name = Path.GetFileName path
                Path = path
                Type = nodeType
            }
            Seq.append
                (Directory.EnumerateDirectories path |> Seq.map (pathToNode "Folder"))
                (Directory.EnumerateFiles path |> Seq.map (pathToNode "File"))
            |> Seq.toList
