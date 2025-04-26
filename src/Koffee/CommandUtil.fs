[<AutoOpen>]
module Koffee.CommandUtil

open FSharp.Control
open VinylUI
open Acadian.FSharp
open Koffee

type ModifierKeys = System.Windows.Input.ModifierKeys
type Key = System.Windows.Input.Key

let actionError actionName = Result.mapError (fun e -> MainStatus.ActionError (actionName, e))
let itemActionError action = Result.mapError (fun e -> MainStatus.ItemActionError (action, e))

let performedAction action (model: MainModel) =
    model
    |> MainModel.pushUndo action
    |> MainModel.withRedoStack []
    |> MainModel.withMessage (MainStatus.ActionComplete action)

let performedUndo undoIter action (model: MainModel) =
    model
    |> MainModel.pushRedo action
    |> MainModel.withMessage (MainStatus.UndoAction (action, undoIter, model.RepeatCount))

let suppressInvalidPathChar char (keyHandler: KeyPressHandler) =
    if Path.InvalidNameChars |> String.contains (string char) then
        keyHandler.Handle()

let parseSearchTerms searchInput =
    searchInput
    |> String.trim
    |> Option.ofString
    |> Option.map (String.split ' ' >> Array.toList)

let filterByTerms sortByStartsWith caseSensitive (terms: string list) projection items =
    let contains = if caseSensitive then String.contains else String.containsIgnoreCase
    let startsWith = if caseSensitive then String.startsWith else String.startsWithIgnoreCase
    let containsAllTerms terms s =
        terms |> List.forall (fun term -> s |> contains term)
    let itemsFilteredByProjection predicate =
        items |> Seq.filter (projection >> predicate)
    if sortByStartsWith then
        Seq.append
            (itemsFilteredByProjection (fun p ->
                p |> startsWith terms.[0] && p |> containsAllTerms (terms |> List.skip 1)
            ))
            (itemsFilteredByProjection (fun p ->
                not (p |> startsWith terms.[0]) && p |> containsAllTerms terms
            ))
    else
        itemsFilteredByProjection (containsAllTerms terms)

let clearSearchProps (model: MainModel) =
    if model.IsSearchingSubFolders then
        model.CancelToken.Cancel()
    { model with
        SearchCurrent = None
        SearchInput = Search.Default
        SearchHistoryIndex = None
        SubDirectories = None
    }

let handleSyncResult (handler: MainModel -> Result<MainModel, _>) model =
    match handler model with
    | Ok newModel -> newModel
    | Error e -> model |> MainModel.withError e

let handleAsyncResult (handler: MainModel -> AsyncSeq<Result<MainModel, _>>) model = asyncSeq {
    let mutable last = model
    for res in handler model |> AsyncSeq.takeWhileInclusive Result.isOk do
        match res with
        | Ok newModel ->
            last <- newModel
            yield newModel
        | Error e ->
            yield last |> MainModel.withError e
}

let SyncResult handler = Sync (handleSyncResult handler)
let AsyncResult handler = Async (handleAsyncResult handler)
