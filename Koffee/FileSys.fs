module Koffee.FileSys

open System.IO
open Koffee

type PathService() =
    interface IPathService with
        override x.root = Path @"C:\"

        override x.parent (Path path) = Path.GetDirectoryName path |> Path

        override x.nodes (Path path) =
            let pathToNode nodeType path = {
                Name = Path.GetFileName path
                Path = Path path
                Type = nodeType
            }
            Seq.append
                (Directory.EnumerateDirectories path |> Seq.map (pathToNode "Folder"))
                (Directory.EnumerateFiles path |> Seq.map (pathToNode "File"))
            |> Seq.toList
