module Koffee.Settings

open System
open System.Windows.Input
open System.Windows.Controls.Primitives
open VinylUI
open VinylUI.Wpf
open Acadian.FSharp
open UIHelpers
open KoffeeUI

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
    | EditSearchExclusions

let private binder (window: SettingsWindow) model =
    window.KeyBindings.AddColumn(<@ fun k -> k.EventName @>, "Command", widthWeight = 3.0)
    window.KeyBindings.AddColumn(<@ fun k -> k.BoundKeys @>, "Bound Keys")

    window.PreviewKeyDown.Add (onKey Key.Escape window.Close)
    window.PreviewKeyDown.Add (onKeyCombo ModifierKeys.Control Key.W window.Close)

    let check (ctl: ToggleButton) =
        ctl.IsChecked <- Nullable true
    check (if model.Config.StartPath = RestorePrevious then window.StartPathPrevious else window.StartPathDefault)
    check (if model.Config.PathFormat = Windows then window.PathFormatWindows else window.PathFormatUnix)

    let isChecked (ic: bool Nullable) = ic.HasValue && ic.Value
    let validatePath = Path.Parse >> Result.ofOption "Invalid path format."

    [   Bind.view(<@ window.DefaultPath.Text @>)
            .toModelResult(<@ model.DefaultPath @>, validatePath, string)
        Bind.view(<@ window.TerminalPath.Text @>).toModel(<@ model.Config.TerminalPath @>)
        Bind.view(<@ window.TextEditor.Text @>).toModel(<@ model.Config.TextEditor @>)

        Bind.view(<@ window.ShowFullPathInTitleBar.IsChecked @>)
            .toModel(<@ model.Config.Window.ShowFullPathInTitle @>, isChecked, Nullable)
        Bind.view(<@ window.ShowHidden.IsChecked @>)
            .toModel(<@ model.Config.ShowHidden @>, isChecked, Nullable)
        Bind.view(<@ window.RefreshOnActivate.IsChecked @>)
            .toModel(<@ model.Config.Window.RefreshOnActivate @>, isChecked, Nullable)

        Bind.model(<@ model.KeyBindings @>).toItemsSource(window.KeyBindings, <@ fun kb -> kb.BoundKeys, kb.EventName @>)
    ]

module Obs = Observable

let private events (window: SettingsWindow) =
    [ window.StartPathPrevious.Checked |> Obs.mapTo (StartPathChanged RestorePrevious)
      window.StartPathDefault.Checked |> Obs.mapTo (StartPathChanged DefaultPath)
      window.DefaultPath.LostFocus |> Obs.mapTo DefaultPathChanged
      window.EditSearchExclusions.Click |> Obs.mapTo EditSearchExclusions
      window.PathFormatWindows.Checked |> Obs.mapTo (PathFormatChanged Windows)
      window.PathFormatUnix.Checked |> Obs.mapTo (PathFormatChanged Unix)
    ]

let updateConfig f (model: Model) =
    { model with Config = f model.Config }

let defaultPathChanged (model: Model) =
    match model.DefaultPath with
    | Ok path -> model |> updateConfig (fun c -> { c with DefaultPath = path })
    | Error _ -> model

let editSearchExclusions (model: Model) =
    let searchExclusions =
        TextEdit.showDialog
            "Recursive Search Exclusions"
            ("This list of folder names are excluded from recursive searches.\n" +
                "Disable exclusions by adding \"/\" in front of them.")
            (model.Config.SearchExclusions @ [""] |> String.concat "\n")
        |> String.split '\n' |> Array.toList |> List.map String.trim
    model |> updateConfig (fun c -> { c with SearchExclusions = searchExclusions })

let private dispatcher evt =
    match evt with
    | StartPathChanged value -> Sync <| updateConfig (fun c -> { c with StartPath = value })
    | DefaultPathChanged -> Sync defaultPathChanged
    | PathFormatChanged value -> Sync <| updateConfig (fun c -> { c with PathFormat = value})
    | EditSearchExclusions -> Sync <| editSearchExclusions

let private start (config: Config) view =
    let keyBinding (evt, name) = {
        EventName = name
        BoundKeys =
            KeyBinding.defaults
            |> List.filter (snd >> ((=) evt))
            |> List.map (fst >> Seq.map KeyBinding.keyDescription >> String.concat "")
            |> String.concat " OR "
    }
    let model = {
        Config = config
        DefaultPath = Ok config.DefaultPath
        KeyBindings = MainCommand.listWithNames |> List.map keyBinding
    }
    Framework.start binder events dispatcher view model

let showDialog parent config =
    SettingsWindow(Owner = parent).ShowDialog(start config).Config
