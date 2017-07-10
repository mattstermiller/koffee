namespace Koffee

open FSharp.Configuration

type Config = YamlConfig<"Config.yaml">

open System
open System.IO
open Reflection
open Utility

type StartupPath =
    | RestorePrevious
    | DefaultPath

module ConfigExt =
    let private filePath =
        let appData = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData)
        Path.Combine(appData, "Koffee", "config.yaml")

    type Config with
        member this.Load () =
            if not <| File.Exists filePath then
                let dir = Path.GetDirectoryName filePath
                if not <| Directory.Exists dir then
                    Directory.CreateDirectory dir |> ignore
                File.WriteAllText(filePath, this.ToString())
            this.Load filePath

        member this.Save () = this.Save filePath

        member this.PathFormat
            with get () = ParseUnionCase<PathFormat> this.PathFormatName |> Option.coalesce Windows
            and set value = this.PathFormatName <- GetUnionCaseName value

        member this.StartupPath
            with get () = ParseUnionCase<StartupPath> this.StartupPathType |> Option.coalesce RestorePrevious
            and set value = this.StartupPathType <- GetUnionCaseName value
