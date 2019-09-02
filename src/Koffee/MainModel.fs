namespace Koffee

open System
open System.Windows.Input
open Acadian.FSharp

type ItemType =
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

type Item = {
    Path: Path
    Name: string
    Type: ItemType
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

    static member Empty =
        { Path = Path.Root; Name = ""; Type = Empty
          Modified = None; Size = None; IsHidden = false; IsSearchMatch = false
        }

    static member Basic path name itemType =
        { Item.Empty with
            Path = path
            Name = name
            Type = itemType
        }


type SortField =
    | Name
    | Type
    | Modified
    | Size

    static member SortByTypeThen (field, desc) (list: Item list) =
        let orderBy, thenBy =
            if desc then Order.by, Order.thenByDesc
            else Order.byDesc, Order.thenBy
        let selector =
            match field with
            | Name -> (fun i -> i.Name.ToLower() :> obj)
            | Type -> (fun i -> i.Type :> obj)
            | Modified -> (fun i -> i.Modified :> obj)
            | Size -> (fun i -> i.Size :> obj)
        list |> orderBy (fun i -> i.Type) |> thenBy selector |> Seq.toList

type RenamePart =
    | Begin
    | EndName
    | End
    | ReplaceName
    | ReplaceAll

type PutAction =
    | Move
    | Copy
    | Shortcut

type PromptType =
    | GoToBookmark
    | SetBookmark
    | DeleteBookmark

type ConfirmType =
    | Overwrite of PutAction * src: Item * dest: Item
    | Delete
    | OverwriteBookmark of char * existingPath: Path

type InputType =
    | Find of multi: bool
    | Search
    | CreateFile
    | CreateFolder
    | Rename of RenamePart

type InputMode =
    | Prompt of PromptType
    | Confirm of ConfirmType
    | Input of InputType

type ItemAction =
    | CreatedItem of Item
    | RenamedItem of Item * newName: string
    | PutItem of PutAction * Item * newPath: Path
    | DeletedItem of Item * permanent: bool

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

type StartPath =
    | RestorePrevious
    | DefaultPath

type WindowConfig = {
    IsMaximized: bool
    Location: int * int
    Size: int * int
    ShowFullPathInTitle: bool
    RefreshOnActivate: bool
}

type Config = {
    StartPath: StartPath
    DefaultPath: Path
    PathFormat: PathFormat
    ShowHidden: bool
    SearchCaseSensitive: bool
    TextEditor: string
    CommandlinePath: string
    YankRegister: (Path * ItemType * PutAction) option
    Window: WindowConfig
    Bookmarks: (char * Path) list
}
with
    member this.GetBookmark char =
        this.Bookmarks |> List.tryFind (fst >> (=) char) |> Option.map snd

    member this.WithBookmark char path =
        let bookmarks =
            this.Bookmarks
            |> List.filter (fst >> (<>) char)
            |> List.append [(char, path)]
            |> List.sortBy (fun (c, _) ->
                // sort an upper case letter immediately after its lower case
                if Char.IsUpper c then Char.ToLower c |> sprintf "%c2" else string c
            )
        { this with Bookmarks = bookmarks }

    member this.WithoutBookmark char =
        { this with Bookmarks = this.Bookmarks |> List.filter (fst >> (<>) char) }

    static member Default = {
        StartPath = RestorePrevious
        DefaultPath = Path.Root
        PathFormat = Windows
        ShowHidden = false
        SearchCaseSensitive = false
        TextEditor = "notepad.exe"
        CommandlinePath = "cmd.exe"
        YankRegister = None
        Window = {
            IsMaximized = false
            Location = (200, 200)
            Size = (800, 800)
            ShowFullPathInTitle = false
            RefreshOnActivate = true
        }
        Bookmarks = []
    }

type History = {
    Paths: Path list
    NetHosts: string list
}
with
    member this.WithPath path =
        { this with Paths = path :: (this.Paths |> List.filter ((<>) path)) |> List.truncate History.MaxPaths }

    member this.WithNetHost host =
        if this.NetHosts |> Seq.exists (String.equalsIgnoreCase host) then
            this
        else
            { this with NetHosts = host :: this.NetHosts |> List.sortBy String.toLower }

    member this.WithoutNetHost host =
        { this with NetHosts = this.NetHosts |> List.filter (not << String.equalsIgnoreCase host) }

    static member MaxPaths = 200

    static member Default = {
        Paths = []
        NetHosts = []
    }

type MainModel = {
    Location: Path
    LocationInput: string
    PathSuggestions: Result<string list, string>
    PathSuggestCache: (Path * Result<Path list, string>) option
    Status: StatusType option
    Items: Item list
    Sort: SortField * bool
    Cursor: int
    PageSize: int
    KeyCombo: KeyCombo
    InputMode: InputMode option
    InputText: string
    InputTextSelection: int * int
    LastFind: string option
    LastSearch: (bool * string) option
    BackStack: (Path * int) list
    ForwardStack: (Path * int) list
    UndoStack: ItemAction list
    RedoStack: ItemAction list
    WindowLocation: int * int
    WindowSize: int * int
    SaveWindowSettings: bool
    Config: Config
    History: History
} with
    member private this.ClampCursor index =
         index |> max 0 |> min (this.Items.Length - 1)

    member this.SelectedItem =
        this.Items.[this.Cursor |> this.ClampCursor]

    member this.PathFormat = this.Config.PathFormat

    member this.LocationFormatted = this.Location.Format this.PathFormat

    member this.HalfPageSize = this.PageSize/2 - 1

    member this.TitleLocation =
        if this.Config.Window.ShowFullPathInTitle then
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
        LocationInput = Path.Root.Format Windows
        PathSuggestions = Ok []
        PathSuggestCache = None
        Status = None
        Items = [ Item.Empty ]
        Sort = Name, false
        Cursor = 0
        PageSize = 30
        KeyCombo = []
        InputMode = None
        InputText = ""
        InputTextSelection = 0, 0
        LastFind = None
        LastSearch = None
        BackStack = []
        ForwardStack = []
        UndoStack = []
        RedoStack = []
        WindowLocation = 0, 0
        WindowSize = 800, 800
        SaveWindowSettings = true
        Config = Config.Default
        History = History.Default
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
    | InputChanged
    | InputDelete of EvtHandler
    | SubmitInput
    | CancelInput
    | StartAction of PutAction
    | ClearYank
    | Put
    | ClipCopy
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
    | PathInputChanged
    | ConfigFileChanged of Config
    | HistoryFileChanged of History
    | PageSizeChanged of int
    | WindowLocationChanged of int * int
    | WindowSizeChanged of int * int
    | WindowMaximizedChanged of bool

    member this.FriendlyName =
        match this with
        | CursorUp -> "Move Cursor Up"
        | CursorDown -> "Move Cursor Down"
        | CursorUpHalfPage -> "Move Cursor Up Half Page"
        | CursorDownHalfPage -> "Move Cursor Down Half Page"
        | CursorToFirst -> "Move Cursor to First Item"
        | CursorToLast -> "Move Cursor to Last Item"
        | StartInput (Find false) -> "Find Item Starting With..."
        | StartInput (Find true) -> "Find Item Starting With... (Multi)"
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
        | StartAction Shortcut -> "Start Create Shortcut to Item"
        | ClearYank -> "Clear Yank Register"
        | Put -> "Put Item to Move/Copy in Current Folder"
        | ClipCopy -> "Copy to Clipboard"
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
        StartInput (Find false)
        StartInput (Find true)
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
        StartAction Shortcut
        ClearYank
        Put
        ClipCopy
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
