namespace Koffee

open System
open FSharp.Desktop.UI
open ConfigExt

type SettingsController(config: Config) =
    interface IController<SettingsEvents, SettingsModel> with
        member this.InitModel model =
            model.KeyBindings <-
                MainEvents.Bindable
                |> List.map (fun evt ->
                    let keys =
                        KeyBinding.DefaultsAsString
                        |> List.choose (fun (key, bound) ->
                            if bound = evt then Some key
                            else None)
                    {
                        EventName = evt.FriendlyName
                        BoundKeys = String.Join(" OR ", keys)
                    })

        member x.Dispatcher = function
            | PathFormatChanged value -> Sync (fun _ ->
                config.PathFormat <- value; config.Save())
            | ShowFullPathInTitleChanged value -> Sync (fun _ ->
                config.Window.ShowFullPathInTitle <- value; config.Save())
