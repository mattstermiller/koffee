namespace Koffee

open System
open System.Windows.Input
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
    | PutItem of PutAction * Node * newPath: Path
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
    InputText: string
    InputTextSelection: int * int
    InputMode: InputMode option
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
    member private this.ClampCursor index =
         index |> max 0 |> min (this.Nodes.Length - 1)

    member this.SelectedNode =
        this.Nodes.[this.Cursor |> this.ClampCursor]

    member this.LocationFormatted = this.Location.Format this.PathFormat

    member this.HalfPageSize = this.PageSize/2 - 1

    member this.TitleLocation =
        if this.ShowFullPathInTitle then
            this.LocationFormatted
        else
            this.Location.Name |> String.ifEmpty this.LocationFormatted

    member this.WithLocation path =
        { this with Location = path; LocationInput = path.FormatFolder this.PathFormat }

    member this.WithCursor index =
        { this with Cursor = index |> this.ClampCursor }

    member this.WithCursorRel move = this.WithCursor (this.Cursor + move)

    static member Default = {
        Location = Path.Root
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
    | StartAction of PutAction
    | Put
    | Recycle
    | Undo
    | Redo
    | SortList of SortField
    | ToggleHidden
    | OpenSplitScreenWindow
    | OpenFileWith
    | OpenProperties
    | OpenWithTextEditor
    | OpenExplorer
    | OpenCommandLine
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
        | StartAction Move -> "Start Move Item"
        | StartAction Copy -> "Start Copy Item"
        | Put -> "Put Item to Move/Copy in Current Folder"
        | Recycle -> "Send to Recycle Bin"
        | Undo -> "Undo Action"
        | Redo -> "Redo Action"
        | SortList field -> sprintf "Sort by %A" field
        | ToggleHidden -> "Show/Hide Hidden Folders and Files"
        | OpenSplitScreenWindow -> "Open New Window for Split Screen"
        | OpenFileWith -> "Open File With..."
        | OpenProperties -> "Open Properties"
        | OpenWithTextEditor -> "Open Selected File With Text Editor"
        | OpenExplorer -> "Open Windows Explorer at Current Location"
        | OpenCommandLine -> "Open Windows Commandline at Current Location"
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
        StartAction Move
        StartAction Copy
        Put
        Recycle
        Undo
        Redo
        SortList Name
        SortList Modified
        SortList Size
        ToggleHidden
        OpenSplitScreenWindow
        OpenFileWith
        OpenProperties
        OpenWithTextEditor
        OpenExplorer
        OpenCommandLine
        OpenSettings
        Exit
    ]
