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
        static member LoadAndConvert (getPathType: Path -> ItemType option) =
            if IO.File.Exists filePath then
                let config = ConfigYaml()
                config.Load filePath |> ignore
                let yankRegister =
                    let path = Path.Parse config.YankRegisterPath
                    let typ = path |> Option.bind getPathType
                    let action = ParseUnionCase<PutAction> config.YankRegisterAction
                    (path, typ, action) |||> Option.map3 (fun p t a -> p, t, a)
                let newConfig =
                    { Config.Default with
                        PathFormat = ParseUnionCase<PathFormat> config.PathFormatName |? Windows
                        StartPath = ParseUnionCase<StartPath> config.StartupPathType |? RestorePrevious
                        DefaultPath = config.DefaultPath |> Path.Parse |? Path.Root
                        ShowHidden = config.ShowHidden
                        SearchCaseSensitive = config.SearchCaseSensitive
                        TextEditor = config.TextEditor
                        CommandlinePath = config.CommandlinePath
                        YankRegister = yankRegister
                        Window = {
                            IsMaximized = config.Window.IsMaximized
                            Location = (config.Window.Left, config.Window.Top)
                            Size = (config.Window.Width, config.Window.Height)
                            ShowFullPathInTitle = config.Window.ShowFullPathInTitle
                            RefreshOnActivate = config.Window.RefreshOnActivate
                        }
                        Bookmarks =
                            config.Bookmarks
                            |> Seq.choose (fun b -> b.Path |> Path.Parse |> Option.map (fun p -> (b.Key.[1], p)))
                            |> Seq.toList
                    }
                let history = {
                    Paths = config.PreviousPath |> Path.Parse |> Option.toList
                    NetHosts = config.NetHosts |> Seq.toList
                }
                Some (newConfig, history)
            else None
