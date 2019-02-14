namespace Koffee

open System
open System.Windows.Input
open FSharp.Desktop.UI
open Acadian.FSharp

type NodeType =
    | File
    | Folder
    | Drive
    | NetHost
    | NetShare
    | Empty

    member this.CanModify =
        match this with
        | File | Folder -> true
        | _ -> false

    member this.CanCreateIn =
        match this with
        | Drive | Folder | NetShare -> true
        | _ -> false

    override this.ToString() =
        match this with
        | NetHost -> "Network Host"
        | NetShare -> "Network Share"
        | _ -> sprintf "%A" this

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

    member this.DisplayName =
        match this.Type with
        | Folder | NetShare -> this.Name + @"\"
        | _ -> this.Name

    member this.Description =
        sprintf "%s \"%s\"" (this.Type.ToString().ToLower()) this.Name

    member this.SizeFormatted = this.Size |> Option.map Format.fileSize |? ""

    static member Empty = {
        Path = Path.Root; Name = ""; Type = Empty
        Modified = None; Size = None; IsHidden = false; IsSearchMatch = false
    }

type SortField =
    | Name
    | Type
    | Modified
    | Size

    static member SortByTypeThen (field, desc) (list: Node list) =
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

type ItemAction =
    | CreatedItem of Node
    | RenamedItem of Node * newName: string
    | MovedItem of Node * newPath: Path
    | CopiedItem of Node * newPath: Path
    | DeletedItem of Node * permanent: bool

type SelectType =
    | SelectNone
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

type MainModel = {
    Location: Path
    PathFormat: PathFormat
    LocationInput: string
    Status: StatusType option
    Nodes: Node list // TODO: change to array for performance
    Sort: SortField * bool
    Cursor: int
    PageSize: int
    ShowHidden: bool
    KeyCombo: KeyCombo
    InputMode: InputMode option
    InputText: string
    InputTextSelection: int * int
    LastFind: (bool * char) option
    LastSearch: (bool * string) option
    BackStack: (Path * int) list
    ForwardStack: (Path * int) list
    YankRegister: (Node * PutAction) option
    UndoStack: ItemAction list
    RedoStack: ItemAction list
    ShowFullPathInTitle: bool
    WindowLocation: int * int
    WindowSize: int * int
    SaveWindowSettings: bool
} with
    member this.SelectedNode =
        let index = min this.Cursor (this.Nodes.Length-1)
        this.Nodes.[index]

    member this.LocationFormatted = this.Location.Format this.PathFormat

    member this.HalfPageSize = this.PageSize/2 - 1

    member this.TitleLocation =
        if this.ShowFullPathInTitle then
            this.LocationFormatted
        else
            this.Location.Name |> String.ifEmpty this.LocationFormatted

    member this.WithLocation path =
        { this with Location = path; LocationInput = path.Format this.PathFormat }

    member this.WithCursor index =
        { this with Cursor = index |> min (this.Nodes.Length - 1) |> max 0 }

    member this.WithCursorRel move = this.WithCursor (this.Cursor + move)

    static member Default =
        { Location = Path.Root
          PathFormat = Windows
          LocationInput = Path.Root.Format Windows
          Status = None
          Nodes = [ Node.Empty ]
          Sort = Name, false
          Cursor = 0
          PageSize = 30
          ShowHidden = false
          KeyCombo = []
          InputMode = None
          InputText = ""
          InputTextSelection = 0, 0
          LastFind = None
          LastSearch = None
          BackStack = []
          ForwardStack = []
          YankRegister = None
          UndoStack = []
          RedoStack = []
          ShowFullPathInTitle = false
          WindowLocation = 0, 0
          WindowSize = 800, 800
          SaveWindowSettings = true
        }

[<AbstractClass>]
type MainBindModel() as this =
    inherit Model()

    do
        this.Path <- Path.Root
        this.PathFormat <- Windows
        this.Nodes <- [ Node.Empty ]
        this.Sort <- Name, false
        this.KeyCombo <- []
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
    abstract LocationInput: string with get, set
    abstract Status: StatusType option with get, set
    abstract Nodes: Node list with get, set
    abstract Sort: SortField * desc: bool with get, set
    abstract Cursor: int with get, set
    abstract PageSize: int with get, set
    abstract ShowHidden: bool with get, set
    abstract KeyCombo: KeyCombo with get, set
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
    abstract SaveWindowSettings: bool with get, set

    member val Invoke = (fun (f: unit -> unit) -> f ()) with get, set

    member this.HasErrorStatus =
        match this.Status with
        | Some (ErrorMessage _) -> true
        | _ -> false

    member this.PathFormatted = this.Path.Format this.PathFormat

    member this.SelectedNode =
        let index = this.Cursor |> min (this.Nodes.Length-1) |> max 0
        this.Nodes.[index]

    member this.HalfPageScroll = this.PageSize/2 - 1

    member this.SetCursor index =
        this.Cursor <- index |> min (this.Nodes.Length - 1) |> max 0

    member this.TitleLocation =
        if this.ShowFullPathInTitle then
            this.PathFormatted
        else
            this.Path.Name |> String.ifEmpty this.PathFormatted

    member this.ToModel () = {
        Location = this.Path
        PathFormat = this.PathFormat
        LocationInput = this.LocationInput
        Status = this.Status
        Nodes = this.Nodes
        Sort = this.Sort
        Cursor = this.Cursor
        PageSize = this.PageSize
        ShowHidden = this.ShowHidden
        KeyCombo = this.KeyCombo
        InputMode = this.InputMode
        InputText = this.InputText
        InputTextSelection = this.InputTextSelection
        LastFind = this.LastFind
        LastSearch = this.LastSearch
        BackStack = this.BackStack
        ForwardStack = this.ForwardStack
        YankRegister = this.YankRegister
        UndoStack = this.UndoStack
        RedoStack = this.RedoStack
        ShowFullPathInTitle = this.ShowFullPathInTitle
        WindowLocation = this.WindowLocation
        WindowSize = this.WindowSize
        SaveWindowSettings = this.SaveWindowSettings
    }

    member this.UpdateFromModel model =
        this.Invoke (fun () ->
            this.Path <- model.Location
            this.PathFormat <- model.PathFormat
            this.LocationInput <- model.LocationInput
            this.Status <- model.Status
            this.Sort <- model.Sort
            this.Nodes <- model.Nodes
            this.Cursor <- model.Cursor
            this.PageSize <- model.PageSize
            this.ShowHidden <- model.ShowHidden
            this.KeyCombo <- model.KeyCombo
            this.InputTextSelection <- model.InputTextSelection
            this.InputText <- model.InputText
            this.InputMode <- model.InputMode
            this.LastFind <- model.LastFind
            this.LastSearch <- model.LastSearch
            this.BackStack <- model.BackStack
            this.ForwardStack <- model.ForwardStack
            this.YankRegister <- model.YankRegister
            this.UndoStack <- model.UndoStack
            this.RedoStack <- model.RedoStack
            this.ShowFullPathInTitle <- model.ShowFullPathInTitle
            this.WindowLocation <- model.WindowLocation
            this.WindowSize <- model.WindowSize
            this.SaveWindowSettings <- model.SaveWindowSettings
        )

type MainEvents =
    | KeyPress of (ModifierKeys * Key) * EvtHandler
    | CursorUp
    | CursorDown
    | CursorUpHalfPage
    | CursorDownHalfPage
    | CursorToFirst
    | CursorToLast
    | FindNext
    | SearchNext
    | SearchPrevious
    | OpenPath of EvtHandler
    | OpenSelected
    | OpenParent
    | Back
    | Forward
    | Refresh
    | StartPrompt of PromptType
    | StartConfirm of ConfirmType
    | StartInput of InputType
    | InputCharTyped of char * EvtHandler
    | InputDelete of EvtHandler
    | SubmitInput
    | CancelInput
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
    | ConfigChanged
    | PageSizeChanged of int
    | WindowLocationChanged of int * int
    | WindowSizeChanged of int * int
    | WindowMaximizedChanged of bool
    | Closed

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
        | OpenPath _ -> "Open Entered Path"
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
        | _ -> ""

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
