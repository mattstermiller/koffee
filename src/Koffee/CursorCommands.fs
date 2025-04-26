module Koffee.CursorCommands

open VinylUI
open Acadian.FSharp
open Koffee
open UIHelpers

let selectToggle (model: MainModel) =
    let cursorItem = model.CursorItem
    let toggle, selectedItems =
        match model.SelectedItems |> List.partition ((=) cursorItem) with
        | [], _ -> (true, model.SelectedItems @ [cursorItem])
        | _, withoutCursorItem -> (false, withoutCursorItem)
    { model with
        SelectedItems = selectedItems
        PreviousSelectIndexAndToggle = Some (model.Cursor, toggle)
    }

let selectRange (model: MainModel) =
    let items = model.Items |> List.toArray
    match model.PreviousSelectIndexAndToggle with
    | Some (prevIndex, toggle) ->
        let rangeItems =
            [prevIndex; model.Cursor]
            |> List.sort
            |> fun indexes -> items.[indexes.[0]..indexes.[1]]
            |> Seq.toList
        let selectedItems =
            if toggle
            then model.SelectedItems @ rangeItems |> List.distinct
            else model.SelectedItems |> List.except rangeItems
        { model with
            SelectedItems = selectedItems
            PreviousSelectIndexAndToggle = Some (model.Cursor, toggle)
        }
    | None ->
        selectToggle model

let scrollView (gridScroller: DataGridScroller) scrollType (model: MainModel) =
    let topIndex =
        match scrollType with
        | CursorTop -> model.Cursor
        | CursorMiddle -> model.Cursor - (model.PageSize/2)
        | CursorBottom -> model.Cursor - model.PageSize + 1
    // unfortunately, a two-way binding on scroll index is not feasible because the ScrollViewer does not exist at
    // binding time, so we're using a side effect :(
    gridScroller.ScrollTo topIndex
    model

let inputFind multi (model: MainModel) =
    { model with
        InputMode = Some (Input (Find multi))
        InputText = ""
    }

let private performFind next reverse prefix model =
    let rotate offset (list: _ list) = list.[offset..] @ list.[0..(offset-1)]
    let indexed = model.Items |> List.indexed
    let items =
        if reverse then
            indexed |> rotate (model.Cursor + (if next then 0 else 1)) |> List.rev
        else
            indexed |> rotate (model.Cursor + (if next then 1 else 0))
    let index =
        items
        |> List.filter (fun (_, item) -> item.Name |> String.startsWithIgnoreCase prefix)
        |> List.skip (model.RepeatCount - 1)
        |> List.tryHead
        |> Option.map fst
    match index with
    | Some index -> { model with InputError = None } |> MainModel.withCursor index
    | None -> { model with InputError = Some (FindFailure prefix) }

let find model =
    if model.InputText |> String.isNotEmpty then
        { model with LastFind = Some model.InputText }
        |> performFind false false model.InputText
    else
        { model with InputError = None }

let findNext model =
    match model.LastFind with
    | Some prefix ->
        model
        |> MainModel.withMessage (MainStatus.Find (prefix, model.RepeatCount))
        |> performFind true false prefix
    | None -> model

type Handler(gridScroller: DataGridScroller) =
    member _.Handle (command: CursorCommand) =
        match command with
        | CursorUp -> Sync (fun m -> m |> MainModel.withCursorRel (-1 * m.RepeatCount))
        | CursorDown -> Sync (fun m -> m |> MainModel.withCursorRel (1 * m.RepeatCount))
        | CursorUpHalfPage -> Sync (fun m -> m |> MainModel.withCursorRel (-m.PageSize/2 * m.RepeatCount))
        | CursorDownHalfPage -> Sync (fun m -> m |> MainModel.withCursorRel (m.PageSize/2 * m.RepeatCount))
        | CursorToFirst -> Sync (fun m -> m |> MainModel.withCursor 0)
        | CursorToLast -> Sync (fun m -> m |> MainModel.withCursor (m.Items.Length - 1))
        | SelectToggle -> Sync selectToggle
        | SelectRange -> Sync selectRange
        | SelectAll -> Sync (fun m -> { m with SelectedItems = m.Items })
        | Scroll scrollType -> Sync (scrollView gridScroller scrollType)
        | StartFind multi -> Sync (inputFind multi)
        | FindNext -> Sync findNext
