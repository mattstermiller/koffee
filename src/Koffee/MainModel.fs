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
        | Empty -> ""
        | _ -> sprintf "%A" this

    member this.Symbol =
        match this with
        | File -> "📄"
        | Folder | NetShare -> "📂"
        | Drive -> "💿"
        | NetHost -> "💻"
        | Empty -> ""

type Item = {
    Path: Path
    Name: string
    Type: ItemType
    Modified: DateTime option
    Size: int64 option
    IsHidden: bool
}
with
    override this.ToString() = this.Path.Format Windows

    member this.Description =
        sprintf "%s \"%s\"" (this.Type.ToString().ToLower()) this.Name

    member this.TypeName = this.Type.ToString()

    member this.SizeFormatted = this.Size |> Option.map Format.fileSize |? ""

    static member Empty =
        { Path = Path.Root; Name = ""; Type = Empty
          Modified = None; Size = None; IsHidden = false
        }

    static member EmptyFolder path =
        let text =
            if path = Path.Network then "Remote hosts that you visit will appear here"
            else "Empty folder"
        [ { Item.Empty with Name = sprintf "<%s>" text; Path = path } ]

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

[<Struct>]
type PathSort = {
    Sort: SortField
    Descending: bool
}
with
    static member Default = { Sort = Name; Descending = false }

    static member ofTuple (sort, descending) = {
        Sort = sort
        Descending = descending
    }

    static member toTuple sort = (sort.Sort, sort.Descending)

type Search = {
    Terms: string
    CaseSensitive: bool
    Regex: bool
    SubFolders: bool
}
with
    static member Default = {
        Terms = ""
        CaseSensitive = false
        Regex = false
        SubFolders = false
    }

type History = {
    Paths: Path list
    Searches: Search list
    NetHosts: string list
    PathSort: Map<Path, PathSort>
}
with
    static member private pushDistinct max list item =
        item :: (list |> List.filter ((<>) item)) |> List.truncate max

    member this.WithPath path =
        { this with Paths = History.pushDistinct History.MaxPaths this.Paths path }

    member this.WithSearch search =
        { this with Searches = History.pushDistinct History.MaxSearches this.Searches search }

    member this.WithNetHost host =
        if this.NetHosts |> Seq.exists (String.equalsIgnoreCase host) then
            this
        else
            { this with NetHosts = host :: this.NetHosts |> List.sortBy String.toLower }

    member this.WithoutNetHost host =
        { this with NetHosts = this.NetHosts |> List.filter (not << String.equalsIgnoreCase host) }

    member this.WithPathAndNetHost path =
        let hist = this.WithPath path
        match path.NetHost with
        | Some host -> hist.WithNetHost host
        | None -> hist

    member this.WithPathSort path sort =
        if sort = PathSort.Default then
            { this with PathSort = this.PathSort.Remove path }
        else
            { this with PathSort = this.PathSort.Add(path, sort) }

    member this.FindSortOrDefault path =
        match this.PathSort.TryFind path with
        | Some sort -> sort
        | None -> PathSort.Default

    static member MaxPaths = 500
    static member MaxSearches = 50

    static member Default = {
        Paths = []
        Searches = []
        NetHosts = []
        PathSort = Map.empty
    }

type CancelToken() =
    let mutable cancelled = false
    member this.IsCancelled = cancelled
    member this.Cancel () = cancelled <- true

type MainModel = {
    Location: Path
    LocationInput: string
    PathSuggestions: Result<string list, string>
    PathSuggestCache: (Path * Result<Path list, string>) option
    Status: StatusType option
    Directory: Item list
    Items: Item list
    Sort: (SortField * bool) option
    Cursor: int
    PageSize: int
    KeyCombo: KeyCombo
    RepeatCommand: int option
    InputMode: InputMode option
    InputText: string
    InputTextSelection: int * int
    LastFind: string option
    SearchInput: Search
    SearchCurrent: Search option
    SubDirectories: Item list option
    SubDirectoryCancel: CancelToken
    SearchHistoryIndex: int
    Progress: float option
    BackStack: (Path * int) list
    ForwardStack: (Path * int) list
    IsNavHistoryVisible: bool
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

    member this.LocationFormatted = this.Location.FormatFolder this.PathFormat

    member this.HalfPageSize = this.PageSize/2 - 1

    member this.IsSearchingSubFolders = this.SearchCurrent |> Option.exists (fun s -> s.SubFolders)

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

    member this.WithPushedLocation path =
        if path <> this.Location then
            { this.WithLocation path with
                BackStack = (this.Location, this.Cursor) :: this.BackStack
                ForwardStack = []
                Cursor = 0
            }
        else this

    member this.WithoutKeyCombo () =
        { this with KeyCombo = []; RepeatCommand = None }

    member this.AppendRepeatDigit digit =
        match this.RepeatCommand with
        | None when digit = 0 -> this
        | Some count -> { this with RepeatCommand = Some (count * 10 + digit) }
        | None -> { this with RepeatCommand = Some digit }

    member this.RepeatCount = this.RepeatCommand |? 1

    static member Default = {
        Location = Path.Root
        LocationInput = Path.Root.Format Windows
        PathSuggestions = Ok []
        PathSuggestCache = None
        Status = None
        Directory = []
        Items = [ Item.Empty ]
        Sort = Some (Name, false)
        Cursor = 0
        PageSize = 30
        KeyCombo = []
        RepeatCommand = None
        InputMode = None
        InputText = ""
        InputTextSelection = 0, 0
        LastFind = None
        SearchInput = Search.Default
        SearchCurrent = None
        SubDirectories = None
        SubDirectoryCancel = CancelToken()
        SearchHistoryIndex = -1
        Progress = None
        BackStack = []
        ForwardStack = []
        IsNavHistoryVisible = false
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
    | OpenPath of string * EvtHandler
    | OpenSelected
    | OpenParent
    | Back
    | Forward
    | ShowNavHistory
    | Refresh
    | DeletePathSuggestion of Path
    | StartPrompt of PromptType
    | StartConfirm of ConfirmType
    | StartInput of InputType
    | InputCharTyped of char * EvtHandler
    | InputChanged
    | InputBack
    | InputForward
    | InputDelete of EvtHandler
    | SubDirectoryResults of Item list
    | AddProgress of float option
    | SubmitInput
    | CancelInput
    | FindNext
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
    | OpenFileAndExit
    | OpenProperties
    | OpenWithTextEditor
    | OpenExplorer
    | OpenCommandLine
    | OpenSettings
    | Exit
    | LocationInputChanged
    | ResetLocationInput
    | ConfigFileChanged of Config
    | HistoryFileChanged of History
    | PageSizeChanged of int
    | WindowLocationChanged of int * int
    | WindowSizeChanged of int * int
    | WindowMaximizedChanged of bool
    | WindowActivated

    static member Bindable = [
        CursorUp, "Move Cursor Up"
        CursorDown, "Move Cursor Down"
        CursorUpHalfPage, "Move Cursor Up Half Page"
        CursorDownHalfPage, "Move Cursor Down Half Page"
        CursorToFirst, "Move Cursor to First Item"
        CursorToLast, "Move Cursor to Last Item"
        StartInput (Find false), "Find Item Starting With..."
        StartInput (Find true), "Find Item Starting With... (Multi)"
        FindNext, "Go To Next Find Match"
        StartInput Search, "Search For Items"
        StartPrompt GoToBookmark, "Go To Bookmark"
        StartPrompt SetBookmark, "Set Bookmark"
        OpenSelected, "Open Selected Item"
        OpenFileWith, "Open File With..."
        OpenFileAndExit, "Open File and Exit"
        OpenProperties, "Open Properties"
        OpenParent, "Open Parent Folder"
        Back, "Back in Navigation History"
        Forward, "Forward in Navigation History"
        ShowNavHistory, "Show Navigation History"
        Refresh, "Refresh Current Folder"
        StartInput CreateFile, "Create File"
        StartInput CreateFolder, "Create Folder"
        StartInput (Rename Begin), "Rename Item (Prepend)"
        StartInput (Rename EndName), "Rename Item (Append to Name)"
        StartInput (Rename End), "Rename Item (Append to Extension)"
        StartInput (Rename ReplaceName), "Rename Item (Replace Name)"
        StartInput (Rename ReplaceAll), "Rename Item (Replace Full Name)"
        StartConfirm Delete, "Delete Permanently"
        StartAction Move, "Start Move Item"
        StartAction Copy, "Start Copy Item"
        StartAction Shortcut, "Start Create Shortcut to Item"
        ClearYank, "Clear Yank Register"
        Put, "Put Item to Move/Copy in Current Folder"
        ClipCopy, "Copy to Clipboard"
        Recycle, "Send to Recycle Bin"
        Undo, "Undo Action"
        Redo, "Redo Action"
        SortList Name, "Sort by Name"
        SortList Modified, "Sort by Modified"
        SortList Size, "Sort by Size"
        ToggleHidden, "Show/Hide Hidden Folders and Files"
        OpenSplitScreenWindow, "Open New Window for Split Screen"
        OpenWithTextEditor, "Open Selected File With Text Editor"
        OpenExplorer, "Open Windows Explorer at Current Location"
        OpenCommandLine, "Open Windows Commandline at Current Location"
        OpenSettings, "Open Help/Settings"
        Exit, "Exit"
    ]
