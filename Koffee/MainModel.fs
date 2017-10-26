namespace Koffee

open System
open FSharp.Desktop.UI
open Utility

type NodeType =
    | File
    | Folder
    | Drive
    | Empty
    | ErrorNode

    override this.ToString() = (sprintf "%A" this).ToLower()

type Node = {
    Path: Path
    Name: string
    Type: NodeType
    Modified: DateTime option
    Size: int64 option
    IsHidden: bool
    IsSearchMatch: bool
}
with
    override this.ToString() = this.Path.Format Windows

    member this.Description =
        sprintf "%O \"%s\"" this.Type this.Name

    member this.SizeFormatted = this.Size |> Option.map Format.fileSize |> Option.defaultValue ""

type SortField =
    | Name
    | Type
    | Modified
    | Size

    static member SortByTypeThen field desc (list: Node list) =
        let orderBy, thenBy =
            if desc then Order.by, Order.thenByDesc
            else Order.byDesc, Order.thenBy
        let selector =
            match field with
            | Name -> (fun n -> n.Name.ToLower() :> obj)
            | Type -> (fun n -> n.Type :> obj)
            | Modified -> (fun n -> n.Modified :> obj)
            | Size -> (fun n -> n.Size :> obj)
        list |> orderBy (fun n -> n.Type) |> thenBy selector |> Seq.toList

type RenamePart =
    | Begin
    | EndName
    | End
    | ReplaceName
    | ReplaceAll

type PutAction =
    | Move
    | Copy

type PromptType =
    | Find of caseSensitive: bool
    | GoToBookmark
    | SetBookmark
    | DeleteBookmark

type ConfirmType =
    | Overwrite of PutAction * src: Node * dest: Node
    | Delete
    | OverwriteBookmark of char * existingPath: Path

type InputType =
    | Search
    | CreateFile
    | CreateFolder
    | Rename of RenamePart

type InputMode =
    | Prompt of PromptType
    | Confirm of ConfirmType
    | Input of InputType
    member this.AllowedOnNodeType nodeType =
        match this with
        | Input (Rename _) ->
            match nodeType with
            | File | Folder -> true
            | _ -> false
        | _ -> true

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
        this.Sort <- Name, false
        this.BackStack <- []
        this.ForwardStack <- []
        this.UndoStack <- []
        this.RedoStack <- []
        this.InputText <- ""
        this.InputTextSelection <- 0, 0
        this.WindowLocation <- 0, 0
        this.WindowSize <- 800, 800

    abstract Path: Path with get, set
    abstract PathFormat: PathFormat with get, set
    abstract Status: StatusType option with get, set
    abstract Nodes: Node list with get, set
    abstract Sort: SortField * desc: bool with get, set
    abstract Cursor: int with get, set
    abstract PageSize: int with get, set
    abstract InputMode: InputMode option with get, set
    abstract InputText: string with get, set
    abstract InputTextSelection: start:int * len:int with get, set
    abstract LastFind: (bool * char) option with get, set
    abstract LastSearch: (bool * string) option with get, set
    abstract BackStack: (Path * int) list with get, set
    abstract ForwardStack: (Path * int) list with get, set
    abstract YankRegister: (Node * PutAction) option with get, set
    abstract UndoStack: ItemAction list with get, set
    abstract RedoStack: ItemAction list with get, set
    abstract ShowFullPathInTitle: bool with get, set
    abstract WindowLocation: int * int with get, set
    abstract WindowSize: int * int with get, set

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
    | StartPrompt of PromptType
    | StartConfirm of ConfirmType
    | StartInput of InputType
    | SubmitInput
    | InputCharTyped of char
    | StartMove
    | StartCopy
    | Put
    | Recycle
    | Undo
    | Redo
    | SortList of SortField
    | ToggleHidden
    | OpenSplitScreenWindow
    | OpenExplorer
    | OpenCommandLine
    | OpenWithTextEditor
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
        | StartPrompt (Find false) -> "Find Item Beginning With Character (case-insensitive)"
        | StartPrompt (Find true) -> "Find Item Beginning With Character (case-sensitive)"
        | FindNext -> "Go To Next Find Match"
        | StartInput Search -> "Search For Items"
        | SearchNext -> "Go To Next Search Match"
        | SearchPrevious -> "Go To Previous Search Match"
        | StartPrompt GoToBookmark -> "Go To Bookmark"
        | StartPrompt SetBookmark -> "Set Bookmark"
        | StartPrompt DeleteBookmark -> "Delete Bookmark"
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
        | StartInput (Rename ReplaceAll) -> "Rename Item (Replace Full Name)"
        | StartConfirm Delete -> "Delete Permanently"
        | StartConfirm _ -> ""
        | InputCharTyped _ -> ""
        | SubmitInput -> "Submit Input for the Current Command"
        | StartMove -> "Start Move Item"
        | StartCopy -> "Start Copy Item"
        | Put -> "Put Item to Move/Copy in Current Folder"
        | Recycle -> "Send to Recycle Bin"
        | Undo -> "Undo Action"
        | Redo -> "Redo Action"
        | SortList field -> sprintf "Sort by %A" field
        | ToggleHidden -> "Show/Hide Hidden Folders and Files"
        | OpenSplitScreenWindow -> "Open New Window for Split Screen"
        | OpenExplorer -> "Open Windows Explorer at Current Location"
        | OpenCommandLine -> "Open Windows Commandline at Current Location"
        | OpenWithTextEditor -> "Open Selected File With Text Editor"
        | OpenSettings -> "Open Help/Settings"
        | Exit -> "Exit"

    static member Bindable = [
        CursorUp
        CursorDown
        CursorUpHalfPage
        CursorDownHalfPage
        CursorToFirst
        CursorToLast
        StartPrompt (Find false)
        StartPrompt (Find true)
        FindNext
        StartInput Search
        SearchNext
        SearchPrevious
        StartPrompt GoToBookmark
        StartPrompt SetBookmark
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
        StartInput (Rename ReplaceAll)
        StartConfirm Delete
        StartMove
        StartCopy
        Put
        Recycle
        Undo
        Redo
        SortList Name
        SortList Modified
        SortList Size
        ToggleHidden
        OpenSplitScreenWindow
        OpenExplorer
        OpenCommandLine
        OpenWithTextEditor
        OpenSettings
        Exit
    ]
