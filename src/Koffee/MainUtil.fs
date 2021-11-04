module Koffee.Main.Util

open Acadian.FSharp
open Koffee

type ModifierKeys = System.Windows.Input.ModifierKeys
type Key = System.Windows.Input.Key

let actionError actionName = Result.mapError (fun e -> ActionError (actionName, e))
let itemActionError item pathFormat = Result.mapError (fun e -> ItemActionError (item, pathFormat, e))

let filterByTerms sort caseSensitive search proj items =
    let terms =
        search
        |> String.trim
        |> Option.ofString
        |> Option.map (String.split ' ' >> Array.toList)
    match terms with
    | Some terms ->
        let contains = if caseSensitive then String.contains else String.containsIgnoreCase
        let startsWith = if caseSensitive then String.startsWith else String.startsWithIgnoreCase
        let matches = items |> List.filter (fun i -> terms |> List.forall (fun t -> proj i |> contains t))
        if sort then
            matches |> List.sortBy (fun i ->
                let weight = if proj i |> startsWith terms.[0] then 0 else 1
                (weight, proj i |> String.toLower)
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
