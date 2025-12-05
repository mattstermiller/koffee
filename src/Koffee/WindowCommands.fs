module Koffee.WindowCommands

open VinylUI
open Acadian.FSharp
open Koffee

let openSplitScreenWindow (os: IOperatingSystem) getScreenBounds model = result {
    let fitRect = Rect.ofPairs model.WindowLocation (model.WindowSize |> mapFst ((*) 2))
                  |> Rect.fit (getScreenBounds())
    let model =
        { model with
            WindowLocation = fitRect.Location
            WindowSize = fitRect.Size |> mapFst (flip (/) 2)
        }

    let left, top = model.WindowLocation
    let width, height = model.WindowSize
    let path = (model.Location.Format Windows).TrimEnd([|'\\'|])
    let args = sprintf "\"%s\" --location=%i,%i --size=%i,%i"
                       path (left + width) top width height

    let! koffeePath = Path.Parse (System.Reflection.Assembly.GetExecutingAssembly().Location)
                      |> Result.ofOption MainStatus.CouldNotFindKoffeeExe
    do! os.Execute false koffeePath.Parent (string koffeePath) args
        |> Result.mapError (fun e -> MainStatus.CouldNotExecute ("Koffee", e))
    return model
}

let openSettings fsReader openSettingsWindow model = result {
    let config = openSettingsWindow model.Config
    return! { model with Config = config } |> NavigationCommands.refresh fsReader
}

type Handler(
    fs: IFileSystemReader,
    os: IOperatingSystem,
    getScreenBounds: unit -> Rectangle,
    openSettingsWindow: Config -> Config,
    closeWindow: unit -> unit
) =
    member _.Handle (command: WindowCommand) =
        match command with
        | OpenSplitScreenWindow -> SyncResult (openSplitScreenWindow os getScreenBounds)
        | OpenSettings -> SyncResult (openSettings fs openSettingsWindow)
        | Exit -> Sync (fun m -> closeWindow(); m)
