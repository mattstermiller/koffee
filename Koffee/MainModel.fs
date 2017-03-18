namespace Koffee

open System
open System.Text.RegularExpressions
open FSharp.Desktop.UI
open Reflection
open ModelExtensions

type NodeType =
    | File
    | Folder
    | Drive
    | Error

    override this.ToString() = (sprintf "%A" this).ToLower()

type Node = {
    Path: Path
    Name: string
    Type: NodeType
    Modified: DateTime option
    Size: int64 option
}
with
    override this.ToString() = this.Path.Format Windows

    member this.Description =
        sprintf "%O \"%s\"" this.Type this.Name

    member this.SizeFormatted =
        if this.Size.IsSome then
            let scale level = pown 1024L level
            let scaleCutoff level = 10L * (scale level)
            let scaledStr size level =
                let scaled = size / (scale level)
                let levelName = "KB,MB,GB".Split(',').[level-1]
                scaled.ToString("N0") + " " + levelName
            match this.Size.Value with
                | size when size > scaleCutoff 3 -> scaledStr size 3
                | size when size > scaleCutoff 2 -> scaledStr size 2
                | size when size > scaleCutoff 1 -> scaledStr size 1
                | size -> size.ToString("N0")
        else ""

type RenamePart =
    | Begin
    | EndName
    | End
    | ReplaceName
    | ReplaceExt

type ConfirmType =
    | Overwrite
    | Delete

type CommandInput =
    | Find
    | Search
    | CreateFile
    | CreateFolder
    | Rename of RenamePart
    | Confirm of ConfirmType

    member this.Prompt (node: Node) =
        match this with
        | Confirm Overwrite -> sprintf "File named \"%s\" already exists, overwrite y/n ?" node.Name
        | Confirm Delete -> sprintf "Permanently delete %s y/n ?" node.Description
        | _ ->
            let caseName = GetUnionCaseName this
            let name = Regex.Replace(caseName, @"(?<=[a-z])(?=[A-Z\d])", " ")
            sprintf "%s:" name

    member this.AllowedOnNodeType nodeType =
        match this with
        | Rename _ ->
            match nodeType with
            | File | Folder -> true
            | _ -> false
        | _ -> true

type BufferAction =
    | Move
    | Copy

type ItemAction =
    | CreatedItem of Node
    | RenamedItem of Node * newName: string
    | MovedItem of Node * newPath: Path
    | CopiedItem of Node * newPath: Path
    | DeletedItem of Node * permanent: bool

type SelectType =
    | KeepSelect
    | SelectIndex of int
    | SelectName of string

type StatusType =
    | Message of string
    | ErrorMessage of string
    | Busy of string

    static member fromExn actionName (ex: exn) =
        let exnMessage =
            match ex with
            | :? AggregateException as agg -> agg.InnerExceptions.[0].Message
            | _ -> ex.Message
        ErrorMessage (sprintf "Could not %s: %s" actionName exnMessage)

[<AbstractClass>]
type MainModel() as this =
    inherit Model()

    do
        this.Path <- Path.Root
        this.PathFormat <- Windows
        this.Nodes <- []
        this.BackStack <- []
        this.ForwardStack <- []
        this.UndoStack <- []
        this.RedoStack <- []
        this.CommandText <- ""
        this.CommandTextSelection <- 0, 0

    abstract Path: Path with get, set
    abstract PathFormat: PathFormat with get, set
    abstract Status: StatusType option with get, set
    abstract Nodes: Node list with get, set
    abstract Cursor: int with get, set
    abstract PageSize: int with get, set
    abstract CommandInputMode: CommandInput option with get, set
    abstract CommandText: string with get, set
    abstract CommandTextSelection: start:int * len:int with get, set
    abstract LastFind: char option with get, set
    abstract LastSearch: string option with get, set
    abstract BackStack: (Path * int) list with get, set
    abstract ForwardStack: (Path * int) list with get, set
    abstract ItemBuffer: (Node * BufferAction) option with get, set
    abstract UndoStack: ItemAction list with get, set
    abstract RedoStack: ItemAction list with get, set

    member this.HasErrorStatus =
        match this.Status with
        | Some (ErrorMessage _) -> true
        | _ -> false

    member this.PathFormatted = this.Path.Format this.PathFormat

    member this.SelectedNode =
        let index = min this.Cursor (this.Nodes.Length-1)
        this.Nodes.[index]

    member this.HalfPageScroll = this.PageSize/2 - 1

    member this.SetCursor index =
        this.Cursor <- index |> min (this.Nodes.Length - 1) |> max 0

type MainEvents =
    | CursorUp
    | CursorDown
    | CursorUpHalfPage
    | CursorDownHalfPage
    | CursorToFirst
    | CursorToLast
    | FindNext
    | SearchNext
    | SearchPrevious
    | OpenPath of string
    | OpenSelected
    | OpenParent
    | Back
    | Forward
    | Refresh
    | StartInput of CommandInput
    | ExecuteCommand
    | CommandCharTyped of char
    | StartMove
    | StartCopy
    | Put
    | Recycle
    | PromptDelete
    | Undo
    | Redo
    | TogglePathFormat
    | OpenExplorer
    | OpenSettings
    | Exit

    member this.FriendlyName =
        match this with
        | CursorUp -> "Move Cursor Up"
        | CursorDown -> "Move Cursor Down"
        | CursorUpHalfPage -> "Move Cursor Up Half Page"
        | CursorDownHalfPage -> "Move Cursor Down Half Page"
        | CursorToFirst -> "Move Cursor to First Item"
        | CursorToLast -> "Move Cursor to Last Item"
        | StartInput Find -> "Find Item Beginning With Character"
        | FindNext -> "Go To Next Find Match"
        | StartInput Search -> "Search For Items"
        | SearchNext -> "Go To Next Search Match"
        | SearchPrevious -> "Go To Previous Search Match"
        | OpenPath path -> sprintf "Open Path \"%s\"" path
        | OpenSelected -> "Open Selected Item"
        | OpenParent -> "Open Parent Folder"
        | Back -> "Back in Location History"
        | Forward -> "Forward in Location History"
        | Refresh -> "Refresh Current Folder"
        | StartInput CreateFile -> "Create File"
        | StartInput CreateFolder -> "Create Folder"
        | StartInput (Rename Begin) -> "Rename Item (Prepend)"
        | StartInput (Rename EndName) -> "Rename Item (Append to Name)"
        | StartInput (Rename End) -> "Rename Item (Append to Extension)"
        | StartInput (Rename ReplaceName) -> "Rename Item (Replace Name)"
        | StartInput (Rename ReplaceExt) -> "Rename Item (Replace Extension)"
        | StartInput (Confirm _) -> ""
        | CommandCharTyped _ -> ""
        | ExecuteCommand -> "Execute the Currently Entered Command"
        | StartMove -> "Start Move Item"
        | StartCopy -> "Start Copy Item"
        | Put -> "Put Item to Move/Copy in Current Folder"
        | Recycle -> "Send to Recycle Bin"
        | PromptDelete -> "Delete Permanently"
        | Undo -> "Undo Action"
        | Redo -> "Redo Action"
        | TogglePathFormat -> "Toggle Between Windows and Unix Path Format"
        | OpenExplorer -> "Open Windows Explorer at Current Location"
        | OpenSettings -> "Open Help/Settings"
        | Exit -> "Exit"

    static member Bindable = [
        CursorUp
        CursorDown
        CursorUpHalfPage
        CursorDownHalfPage
        CursorToFirst
        CursorToLast
        StartInput Find
        FindNext
        StartInput Search
        SearchNext
        SearchPrevious
        OpenSelected
        OpenParent
        Back
        Forward
        Refresh
        StartInput CreateFile
        StartInput CreateFolder
        StartInput (Rename Begin)
        StartInput (Rename EndName)
        StartInput (Rename End)
        StartInput (Rename ReplaceName)
        StartInput (Rename ReplaceExt)
        StartMove
        StartCopy
        Put
        Recycle
        PromptDelete
        Undo
        Redo
        TogglePathFormat
        OpenExplorer
        OpenSettings
        Exit
    ]
