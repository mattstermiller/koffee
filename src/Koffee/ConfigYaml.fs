namespace Koffee

open System
open Reflection
open Acadian.FSharp
open FSharp.Configuration

type ConfigYaml = YamlConfig<"config.yaml">

[<AutoOpen>]
module ConfigYamlExt =
    let private filePath = Path.KoffeeData.Join("config.yaml").Format Windows

    type ConfigYaml with
        static member LoadAndConvert (getPathType: Path -> NodeType option) =
            if IO.File.Exists filePath then
                let config = ConfigYaml()
                config.Load filePath |> ignore
                let yankRegister =
                    let path = Path.Parse config.YankRegisterPath
                    let typ = path |> Option.bind getPathType
                    let action = ParseUnionCase<PutAction> config.YankRegisterAction
                    (path, typ, action) |||> Option.map3 (fun p t a -> p, t, a)
                Some {
                    PathFormat = ParseUnionCase<PathFormat> config.PathFormatName |? Windows
                    StartPath = ParseUnionCase<StartPath> config.StartupPathType |? RestorePrevious
                    DefaultPath = config.DefaultPath
                    PreviousPath = config.PreviousPath
                    ShowHidden = config.ShowHidden
                    SearchCaseSensitive = config.SearchCaseSensitive
                    TextEditor = config.TextEditor
                    CommandlinePath = config.CommandlinePath
                    YankRegister = yankRegister
                    Window = {
                        IsMaximized = config.Window.IsMaximized
                        Location = (config.Window.Left, config.Window.Top)
                        Size = (config.Window.Width, config.Window.Height)
                        ShowFullPathInTitle = false
                        RefreshOnActivate = true
                    }
                    Bookmarks =
                        config.Bookmarks
                        |> Seq.choose (fun b -> b.Path |> Path.Parse |> Option.map (fun p -> (b.Key.[1], p)))
                        |> Seq.toList
                    NetHosts = config.NetHosts |> Seq.toList
                }
            else None
