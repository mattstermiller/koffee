namespace Koffee

open System.Diagnostics

type IOperatingSystem =
    abstract member OpenFile: Path -> Result<unit, exn>
    abstract member OpenExplorer: Node -> unit
    abstract member LaunchApp: exePath: string -> workingPath: Path -> args: string -> Result<unit, exn>

type OperatingSystem() =
    let wpath (path: Path) = path.Format Windows

    interface IOperatingSystem with
        member this.OpenFile path =
            tryResult <| fun () ->
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
            tryResult <| fun () ->
                ProcessStartInfo(exePath, args, WorkingDirectory = wpath workingPath)
                |> Process.Start |> ignore
