module Koffee.Main.Util

open Acadian.FSharp
open Koffee

type ModifierKeys = System.Windows.Input.ModifierKeys
type Key = System.Windows.Input.Key

let actionError actionName = Result.mapError (fun e -> MainStatus.ActionError (actionName, e))
let itemActionError action pathFormat = Result.mapError (fun e -> MainStatus.ItemActionError (action, pathFormat, e))

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
