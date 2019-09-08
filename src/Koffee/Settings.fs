module Koffee.Settings

open System
open System.Windows.Input
open System.Windows.Controls.Primitives
open VinylUI
open VinylUI.Wpf
open Acadian.FSharp

type KeyBind = {
    EventName: string
    BoundKeys: string
}

type Model = {
    Config: Config
    DefaultPath: Result<Path, string>
    KeyBindings: KeyBind list
}

type Events =
    | StartPathChanged of StartPath
    | DefaultPathChanged
    | PathFormatChanged of PathFormat

type View = FsXaml.XAML<"SettingsWindow.xaml">

let private binder (view: View) model =
    view.KeyBindings.AddColumn(<@ fun k -> k.EventName @>, "Command", widthWeight = 3.0)
    view.KeyBindings.AddColumn(<@ fun k -> k.BoundKeys @>, "Bound Keys")

    view.PreviewKeyDown.Add (onKey Key.Escape view.Close)
    view.PreviewKeyDown.Add (onKeyCombo ModifierKeys.Control Key.W view.Close)

    let check (ctl: ToggleButton) =
        ctl.IsChecked <- Nullable true
    check (if model.Config.StartPath = RestorePrevious then view.StartPathPrevious else view.StartPathDefault)
    check (if model.Config.PathFormat = Windows then view.PathFormatWindows else view.PathFormatUnix)

    let isChecked (ic: bool Nullable) = ic.HasValue && ic.Value
    let validatePath = Path.Parse >> Result.ofOption "Invalid path format."

    [   Bind.view(<@ view.DefaultPath.Text @>)
            .toModelResult(<@ model.DefaultPath @>, validatePath, string)
        Bind.view(<@ view.CommandlinePath.Text @>).toModel(<@ model.Config.CommandlinePath @>)
        Bind.view(<@ view.TextEditor.Text @>).toModel(<@ model.Config.TextEditor @>)

        Bind.view(<@ view.ShowFullPathInTitleBar.IsChecked @>)
            .toModel(<@ model.Config.Window.ShowFullPathInTitle @>, isChecked, Nullable)
        Bind.view(<@ view.ShowHidden.IsChecked @>)
            .toModel(<@ model.Config.ShowHidden @>, isChecked, Nullable)
        Bind.view(<@ view.RefreshOnActivate.IsChecked @>)
            .toModel(<@ model.Config.Window.RefreshOnActivate @>, isChecked, Nullable)

        Bind.model(<@ model.KeyBindings @>).toItemsSource(view.KeyBindings, <@ fun kb -> kb.BoundKeys, kb.EventName @>)
    ]

let private events (view: View) =
    [ view.StartPathPrevious.Checked |> Observable.mapTo (StartPathChanged RestorePrevious)
      view.StartPathDefault.Checked |> Observable.mapTo (StartPathChanged DefaultPath)
      view.DefaultPath.LostFocus |> Observable.mapTo DefaultPathChanged

      view.PathFormatWindows.Checked |> Observable.mapTo (PathFormatChanged Windows)
      view.PathFormatUnix.Checked |> Observable.mapTo (PathFormatChanged Unix)
    ]

let updateConfig f (model: Model) =
    { model with Config = f model.Config }

let defaultPathChanged (model: Model) =
    match model.DefaultPath with
    | Ok path -> model |> updateConfig (fun c -> { c with DefaultPath = path })
    | Error _ -> model

let private dispatcher evt =
    match evt with
    | StartPathChanged value -> Sync <| updateConfig (fun c -> { c with StartPath = value })
    | DefaultPathChanged -> Sync defaultPathChanged
    | PathFormatChanged value -> Sync <| updateConfig (fun c -> { c with PathFormat = value})

let start (config: Config) view =
    let keyBinding (evt: MainEvents) = {
        EventName = evt.FriendlyName
        BoundKeys =
            KeyBinding.defaults
            |> List.filter (snd >> ((=) evt))
            |> List.map (fst >> Seq.map KeyBinding.keyDescription >> String.concat "")
            |> String.concat " OR "
    }
    let model = {
        Config = config
        DefaultPath = Ok config.DefaultPath
        KeyBindings = MainEvents.Bindable |> List.map keyBinding
    }
    Framework.start binder events dispatcher view model
