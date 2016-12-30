namespace Koffee

open System
open System.Text.RegularExpressions
open FSharp.Desktop.UI
open Koffee.Reflection
open ModelExtensions

type Path =
    | Path of string
    member this.Value =
        let (Path p) = this
        p

type NodeType =
    | File
    | Folder
    | Drive
    | Error

type Node = {
    Path: Path
    Name: string
    Type: NodeType
    Modified: DateTime option
    Size: int64 option
}

type Node with
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

type CursorPosition =
    | Begin
    | End
    | Replace

type CommandInput =
    | Find
    | Search
    | CreateFile
    | CreateFolder
    | DeletePermanently
    | Rename of CursorPosition

    member this.Prompt (node: Node) =
        match this with
        | DeletePermanently -> sprintf "Permanently delete \"%s\" y/n ?" node.Name
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

type ItemAction =
    | CreatedItem of Node
    | RenamedItem of Node * newName:string
    | DeletedItem of Node * permanent:bool


[<AbstractClass>]
type MainModel() as this =
    inherit Model()

    do
        this.OnPropertyChanged <@ this.Status @> (fun _ -> this.IsErrorStatus <- false)
        this.BackStack <- []
        this.ForwardStack <- []

    abstract Path: Path with get, set
    abstract Status: string with get, set
    abstract IsErrorStatus: bool with get, set
    abstract Nodes: Node list with get, set
    abstract Cursor: int with get, set
    abstract PageSize: int with get, set
    abstract CommandInputMode: CommandInput option with get, set
    abstract CommandText: string with get, set
    abstract CommandTextSelection: int * int with get, set
    abstract LastFind: char option with get, set
    abstract LastSearch: string option with get, set
    abstract BackStack: (Path * int) list with get, set
    abstract ForwardStack: (Path * int) list with get, set

    member this.SetErrorStatus status =
        this.Status <- status
        this.IsErrorStatus <- true

    member this.SetExceptionStatus (ex: Exception) action =
        this.SetErrorStatus (sprintf "Could not %s: %s" action ex.Message)

    member this.SelectedNode =
        let index = min this.Cursor (this.Nodes.Length-1)
        this.Nodes.[index]

    member this.FindNode name = List.findIndex (fun n -> n.Name = name) this.Nodes

    member this.HalfPageScroll = this.PageSize/2 - 1

type MainEvents =
    | CursorUp
    | CursorDown
    | CursorUpHalfPage
    | CursorDownHalfPage
    | CursorToFirst
    | CursorToLast
    | OpenPath of string
    | OpenSelected
    | OpenParent
    | Back
    | Forward
    | Refresh
    | StartInput of CommandInput
    | ExecuteCommand
    | CommandCharTyped of char
    | FindNext
    | SearchNext
    | SearchPrevious
    | Delete
    | TogglePathFormat
    | OpenSettings
    | OpenExplorer

    member this.FriendlyName =
        match this with
        | CursorUp -> "Move Cursor Up"
        | CursorDown -> "Move Cursor Down"
        | CursorUpHalfPage -> "Move Cursor Up Half Page"
        | CursorDownHalfPage -> "Move Cursor Down Half Page"
        | CursorToFirst -> "Move Cursor to First Item"
        | CursorToLast -> "Move Cursor to Last Item"
        | OpenPath path -> sprintf "Open Path \"%s\"" path
        | OpenSelected -> "Open Selected Item"
        | OpenParent -> "Open Parent Folder"
        | Back -> "Back in Location History"
        | Forward -> "Forward in Location History"
        | Refresh -> "Refresh Current Folder"
        | StartInput CreateFile -> "Create File"
        | StartInput CreateFolder -> "Create Folder"
        | StartInput DeletePermanently -> "Delete Permanently"
        | StartInput (Rename Begin) -> "Rename File (Prepend)"
        | StartInput (Rename End) -> "Rename File (Append)"
        | StartInput (Rename Replace) -> "Rename File (Replace)"
        | StartInput Find -> "Find Item Beginning With Character"
        | StartInput Search -> "Search For Items"
        | ExecuteCommand -> "Execute the Currently Entered Command"
        | CommandCharTyped char -> sprintf "Find Item Beginning With \"%c\"" char
        | FindNext -> "Go To Next Find Match"
        | SearchNext -> "Go To Next Search Match"
        | SearchPrevious -> "Go To Previous Search Match"
        | Delete -> "Send to Recycle Bin"
        | TogglePathFormat -> "Toggle Between Windows and Unix Path Format"
        | OpenSettings -> "Open Help/Settings"
        | OpenExplorer -> "Open Windows Explorer at Current Location"

    static member Bindable = [
        CursorUp
        CursorDown
        CursorUpHalfPage
        CursorDownHalfPage
        CursorToFirst
        CursorToLast
        OpenSelected
        OpenParent
        Back
        Forward
        Refresh
        StartInput CreateFile
        StartInput CreateFolder
        StartInput (Rename Begin)
        StartInput (Rename End)
        StartInput (Rename Replace)
        StartInput Find
        FindNext
        StartInput Search
        SearchNext
        SearchPrevious
        Delete
        TogglePathFormat
        OpenSettings
        OpenExplorer
    ]
