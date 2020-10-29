module Koffee.Prompts

open VinylUI

type ModifierKeys = System.Windows.Input.ModifierKeys
type Key = System.Windows.Input.Key

module ClosablePrompt =
    let closePrompt handleEvt (model: MainModel) =
        handleEvt()
        model.WithoutPrompt()

    let isClosePromptChord chord =
        chord = (ModifierKeys.None, Key.Escape)

    let (|ClosePromptKeyPress|_|) (evt: MainEvents) =
        match evt with
        | KeyPress (chord, handler) when isClosePromptChord chord -> Some handler
        | _ -> None

    let dispatcher fallbackDisp evt =
        match evt with
        | ClosePromptKeyPress handler -> Sync (closePrompt handler.Handle)
        | evt -> fallbackDisp evt

module DeleteBookmarkPrompt =
    let deleteBookmark char handleEvt model = asyncSeq {
        handleEvt()
        match model.Config.GetBookmark char with
        | Some path ->
            yield
                { model with
                    Config = model.Config.WithoutBookmark char
                    Status = Some <| MainStatus.deletedBookmark char (path.Format model.PathFormat)
                }
        | None ->
            yield { model with Status = Some <| MainStatus.noBookmark char }
    }

    let dispatcher fallback evt =
        match evt with
        | InputCharTyped (c, handler) -> Async (deleteBookmark c handler.Handle)
        | _ -> ClosablePrompt.dispatcher fallback evt

module BaseBookmarkPrompt =
    let switchToDeleteBookmarkPrompt handleEvt model =
        handleEvt()
        { model with PromptDispatcher = Some DeleteBookmarkPrompt.dispatcher }

    let dispatcher fallback evt =
        match evt with
        | InputDelete handler -> Sync (switchToDeleteBookmarkPrompt handler.Handle)
        | _ -> ClosablePrompt.dispatcher fallback evt

module GoToBookmarkPrompt =
    let goToBookmark char handleEvt model = asyncSeq {
        handleEvt()
        match model.Config.GetBookmark char with
        | Some path ->
            yield model
            //yield! Nav.openPath fs path SelectNone model
        | None ->
            yield { model with Status = Some <| MainStatus.noBookmark char }
    }

    let dispatcher fallback evt =
        match evt with
        | InputCharTyped (c, handler) -> Async (goToBookmark c handler.Handle)
        | _ -> BaseBookmarkPrompt.dispatcher fallback evt

module SetBookmarkPrompt =
    let withBookmark char model =
        { model with
            Config = model.Config.WithBookmark char model.Location
            Status = Some <| MainStatus.setBookmark char model.LocationFormatted
        }

    let setBookmark char model = asyncSeq {
        match model.Config.GetBookmark char with
        | Some existingPath ->
            yield
                { model with
                    InputMode = Some (Confirm (OverwriteBookmark (char, existingPath)))
                    InputText = ""
                }
        | None ->
            yield withBookmark char model
    }

    let dispatcher fallback evt =
        match evt with
        | InputCharTyped (c, _) -> Async (setBookmark c)
        | _ -> BaseBookmarkPrompt.dispatcher fallback evt

