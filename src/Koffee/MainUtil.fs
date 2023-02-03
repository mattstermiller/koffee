module Koffee.Main.Util

open Acadian.FSharp
open Koffee

type ModifierKeys = System.Windows.Input.ModifierKeys
type Key = System.Windows.Input.Key

let actionError actionName = Result.mapError (fun e -> MainStatus.ActionError (actionName, e))
let itemActionError action pathFormat = Result.mapError (fun e -> MainStatus.ItemActionError (action, pathFormat, e))

let filterByTerms sortByStartsWith caseSensitive search projection items =
    let terms =
        search
        |> String.trim
        |> Option.ofString
        |> Option.map (String.split ' ' >> Array.toList)
    match terms with
    | Some terms ->
        let contains = if caseSensitive then String.contains else String.containsIgnoreCase
        let startsWith = if caseSensitive then String.startsWith else String.startsWithIgnoreCase
        let matches = items |> List.filter (fun i -> terms |> List.forall (fun t -> projection i |> contains t))
        if sortByStartsWith then
            matches |> List.sortBy (fun i ->
                if projection i |> startsWith terms.[0] then 0 else 1
            )
        else
            matches
    | None -> items

let clearSearchProps model =
    model.SubDirectoryCancel.Cancel()
    { model with
        SearchCurrent = None
        SearchInput = Search.Default
        SearchHistoryIndex = None
        SubDirectories = None
    }
