namespace Koffee

open System
open FSharp.Desktop.UI
open Utility
open ConfigExt

type SettingsController(config: Config) =
    interface IController<SettingsEvents, SettingsModel> with
        member x.InitModel model =
            model.KeyBindings <-
                MainEvents.Bindable
                |> List.map (fun evt ->
                    { EventName = evt.FriendlyName
                      BoundKeys =
                        KeyBinding.DefaultsAsString
                        |> List.filter (snd >> ((=) evt))
                        |> List.map fst
                        |> Str.join " OR "
                    })

        member x.Dispatcher = function
            | StartupPathChanged value -> Sync (fun _ ->
                config.StartupPath <- value; config.Save())
            | DefaultPathChanged value -> Sync (fun _ ->
                config.DefaultPath <- value; config.Save())
            | TextEditorChanged value -> Sync (fun _ ->
                config.TextEditor <- value; config.Save())
            | PathFormatChanged value -> Sync (fun _ ->
                config.PathFormat <- value; config.Save())
            | ShowFullPathInTitleChanged value -> Sync (fun _ ->
                config.Window.ShowFullPathInTitle <- value; config.Save())
            | ShowHiddenChanged value -> Sync (fun _ ->
                config.ShowHidden <- value; config.Save())
