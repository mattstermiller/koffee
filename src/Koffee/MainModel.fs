namespace Koffee

open System
open System.Windows
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

    member this.ToLowerString() = this.ToString().ToLower()

    member this.Symbol =
        match this with
        | File -> "📄"
        | Folder | NetShare -> "📂"
        | Drive -> "💿"
        | NetHost -> "💻"
        | Empty -> ""

type ItemRef = Path * ItemType

module ItemRef =
    let describe ((path, typ): ItemRef) =
        sprintf "%s \"%s\"" (typ.ToLowerString()) path.Name

type Item = {
    Path: Path
    Name: string
    Type: ItemType
    Modified: DateTime option
    Size: int64 option
    IsHidden: bool
}
with
    override this.ToString() = sprintf "%O at %O" this.Type this.Path

    member this.Ref = (this.Path, this.Type)

    member this.Description = ItemRef.describe this.Ref

    member this.TypeName = this.Type.ToString()

    member this.SizeFormatted = this.Size |> Option.map Format.fileSize |? ""

    static member Empty =
        { Path = Path.Root; Name = ""; Type = Empty
          Modified = None; Size = None; IsHidden = false
        }

    static member EmptyFolderWithMessage message path =
        [ { Item.Empty with Name = sprintf "<%s>" message; Path = path } ]

    static member EmptyFolder isSearching path =
        let message =
            if isSearching then "No search results"
            else if path = Path.Network then "Remote hosts that you visit will appear here"
            else "Empty folder"
        Item.EmptyFolderWithMessage message path

    static member Basic path name itemType =
        { Item.Empty with
            Path = path
            Name = name
            Type = itemType
        }

    static member paths (items: Item list) = items |> List.map (fun i -> i.Path)

    static member describeList (items: Item list) =
        match items with
        | [item] -> item.Description
        | _ -> sprintf "%s items" (items.Length |> String.format "N0")

type SortField =
    | Name
    | Type
    | Modified
    | Size

    static member SortByTypeThen (field, desc) (list: Item list) =
        let comparer selector desc a b =
            let aVal, bVal = selector a, selector b
            if aVal = bVal then 0
            else if aVal < bVal = not desc then -1
            else 1
        list |> List.sortWith (fun a b ->
            match comparer (fun i -> i.Type) (not desc) a b with
            | 0 ->
                let compare =
                    match field with
                    | Name -> comparer (fun i -> i.Name.ToLower())
                    | Type -> comparer (fun i -> i.Type)
                    | Modified -> comparer (fun i -> i.Modified)
                    | Size -> comparer (fun i -> i.Size)
                compare desc a b
            | res -> res
        )

type RenamePart =
    | Begin
    | EndName
    | End
    | ReplaceName
    | ReplaceAll

type PutType =
    | Move
    | Copy
    | Shortcut
with
    member this.ToLowerString() = this.ToString().ToLower()

type Search = {
    Terms: string
    CaseSensitive: bool
    Regex: bool
    SubFolders: bool
}
with
    override this.ToString () =
        let flags = [
            if this.CaseSensitive then "Case"
            if this.Regex then "Regex"
            if this.SubFolders then "Sub"
        ]
        let flagStr = if flags |> List.isEmpty then "" else flags |> String.concat ", " |> sprintf " (%s)"
        this.Terms + flagStr

    static member Default = {
        Terms = ""
        CaseSensitive = false
        Regex = false
        SubFolders = false
    }

type PromptType =
    | GoToBookmark
    | SetBookmark
    | DeleteBookmark
    | GoToSavedSearch
    | SetSavedSearch
    | DeleteSavedSearch

type ConfirmType =
    | Overwrite of PutType * src: Item * dest: Item
    | Delete
    | OverwriteBookmark of char * existingPath: Path
    | OverwriteSavedSearch of char * existingSearch: Search

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

type PutItem = {
    ItemType: ItemType
    Source: Path
    Dest: Path
    DestExists: bool
}
with
    static member intentEquals (a: PutItem) (b: PutItem) =
        a.Source = b.Source &&
        a.Dest = b.Dest &&
        a.ItemType = b.ItemType

    static member reverse putItem =
        { putItem with Source = putItem.Dest; Dest = putItem.Source }

    static member describeList pathFormat (putItems: PutItem list) =
        match putItems with
        | [putItem] ->
            sprintf "%s to \"%s\"" (ItemRef.describe (putItem.Source, putItem.ItemType)) (putItem.Dest.Format pathFormat)
        | {Dest = dest} :: _ ->
            sprintf "%s items to %s" (putItems.Length |> String.format "N0") (dest.Parent.Format pathFormat)
        | [] -> "0 items"

type ItemAction =
    | CreatedItem of Item
    | RenamedItem of Item * newName: string
    | PutItems of PutType * intent: PutItem * actual: PutItem list * cancelled: bool
    | DeletedItems of permanent: bool * Item list * cancelled: bool
with
    member this.Description pathFormat =
        match this with
        | CreatedItem item -> sprintf "Create %s" item.Description
        | RenamedItem (item, newName) -> sprintf "Rename %s to %s" item.Description newName
        | PutItems (putType, intent, actual, _) ->
            let fileCountStr =
                actual
                |> Seq.filter (fun pi -> pi.ItemType = File)
                |> Seq.length
                |> function
                    | 0 -> ""
                    | count -> sprintf " (%i files)" count
            sprintf "%O %s%s" putType ([intent] |> PutItem.describeList pathFormat) fileCountStr
        | DeletedItems (permanent, items, _) ->
            match items with
            | item :: rest ->
                let action = if permanent then "Delete" else "Recycle"
                let restDescr = sprintf " and %i others" rest.Length
                let sizeDescr =
                    items
                    |> List.choose (fun item -> item.Size)
                    |> Option.ofCond (not << List.isEmpty)
                    |> Option.map (List.sum >> Format.fileSize >> sprintf " (%s)")
                    |? ""
                sprintf "%s %s%s (%s)" action item.Description restDescr sizeDescr
            | [] -> "" // items should never be empty

type CursorMoveType =
    | CursorStay
    | CursorToIndex of int
    | CursorToName of string
    | CursorToItem of Item * showHidden: bool

type InputError =
    | FindFailure of prefix: string
    | InvalidRegex of error: string
    member this.Message =
        match this with
        | FindFailure prefix -> sprintf "No item starts with \"%s\"" prefix
        | InvalidRegex error -> sprintf "Invalid Regular Expression: %s" error

type UndoMoveBlockedByExistingItemException() =
    inherit exn("An item already exists in the previous location")

type RedoPutBlockedByExistingItemException() =
    inherit exn("An item already exists in the destination")

[<RequireQualifiedAccess>]
module MainStatus =
    let private actionCompleteMessage action pathFormat =
        match action with
        | CreatedItem item ->
            sprintf "Created %s" item.Description
        | RenamedItem (item, newName) ->
            sprintf "Renamed %s to \"%s\"" item.Description newName
        | PutItems (Move, item, _, _) ->
            sprintf "Moved %s" ([item] |> PutItem.describeList pathFormat)
        | PutItems (Copy, item, _, _) ->
            sprintf "Copied %s" ([item] |> PutItem.describeList pathFormat)
        | PutItems (Shortcut, {ItemType = typ; Source = src}, _, _) ->
            sprintf "Created shortcut to %s \"%s\"" (typ.ToLowerString()) (src.Format pathFormat)
        | DeletedItems (false, items, _) ->
            sprintf "Sent %s to Recycle Bin" (items |> Item.describeList)
        | DeletedItems (true, items, _) ->
            sprintf "Deleted %s" (items |> Item.describeList)

    type Message =
        // Navigation
        | Find of prefix: string * repeatCount: int
        | NoBookmark of Char
        | SetBookmark of Char * path: string
        | DeletedBookmark of Char * path: string
        | NoSavedSearch of Char
        | SetSavedSearch of Char * Search
        | DeletedSavedSearch of Char * Search

        // Actions
        | ActionComplete of ItemAction * PathFormat
        | UndoAction of ItemAction * PathFormat * repeatIter: int * repeatCount: int
        | RedoAction of ItemAction * PathFormat * repeatIter: int * repeatCount: int
        | CancelledConfirm of ConfirmType
        | CancelledPut of PutType * isUndo: bool * completed: int * total: int
        | CancelledDelete of permanent: bool * completed: int * total: int
        | Sort of field: obj * desc: bool
        | ToggleHidden of showing: bool
        | OpenFiles of names: string list
        | OpenProperties of name: string
        | OpenExplorer
        | OpenCommandLine of path: string
        | OpenTextEditor of name: string
        | ClipboardCopy of path: string
        | RemovedNetworkHosts of names: string list

        member private this.DescribeCancelledProgress action completed total =
            if completed = 0
            then "nothing done"
            else sprintf "%i of %i already %s" completed total action

        member this.Message =
            match this with
            | Find (prefix, repeatCount) ->
                if repeatCount = 1
                then sprintf "Find item starting with: %s" prefix
                else sprintf "Find every %i items starting with: %s" repeatCount prefix
            | NoBookmark char ->
                sprintf "Bookmark \"%c\" not set" char
            | SetBookmark (char, path) ->
                sprintf "Set bookmark \"%c\" to %s" char path
            | DeletedBookmark (char, path) ->
                sprintf "Deleted bookmark \"%c\" that was set to %s" char path
            | NoSavedSearch char ->
                sprintf "Saved Search \"%c\" not set" char
            | SetSavedSearch (char, search) ->
                sprintf "Set saved search \"%c\" to \"%O\"" char search
            | DeletedSavedSearch (char, search) ->
                sprintf "Deleted saved search \"%c\" that was set to \"%O\"" char search

            | ActionComplete (action, pathFormat) ->
                actionCompleteMessage action pathFormat
            | UndoAction (action, pathFormat, repeatIter, repeatCount) ->
                let prefix =
                    if repeatCount = 1
                    then "Action undone: "
                    else sprintf "Action %i of %i undone: " repeatIter repeatCount
                prefix + actionCompleteMessage action pathFormat
            | RedoAction (action, pathFormat, repeatIter, repeatCount) ->
                let prefix =
                    if repeatCount = 1
                    then "Action redone: "
                    else sprintf "Action %i of %i redone: " repeatIter repeatCount
                prefix + actionCompleteMessage action pathFormat
            | CancelledConfirm confirmType ->
                let action =
                    match confirmType with
                    | Delete -> "delete"
                    | Overwrite _ -> "overwrite item"
                    | OverwriteBookmark _ -> sprintf "overwrite bookmark"
                    | OverwriteSavedSearch _ -> sprintf "overwrite saved search"
                sprintf "Cancelled %s" action
            | CancelledPut (putType, isUndo, completed, total) ->
                let undo = if isUndo then "undo of " else ""
                let action =
                    match putType, isUndo with
                    | Move, _ -> "moved"
                    | _, false -> "copied"
                    | _, true -> "deleted"
                sprintf "Cancelled %s%s - %s" undo (putType.ToLowerString())
                    (this.DescribeCancelledProgress action completed total)
            | CancelledDelete (permanent, completed, total) ->
                let action = if permanent then "delete" else "recycle"
                sprintf "Cancelled %s - %s" action (this.DescribeCancelledProgress (action+"d") completed total)
            | Sort (field, desc) ->
                sprintf "Sort by %A %s" field (if desc then "descending" else "ascending")
            | ToggleHidden showing ->
                sprintf "%s hidden files" (if showing then "Showing" else "Hiding")
            | OpenFiles names ->
                match names with
                | [name] -> sprintf "Opened File: %s" name
                | _ -> sprintf "Opened %i Files" names.Length
            | OpenProperties name ->
                sprintf "Opened Properties: %s" name
            | OpenExplorer ->
                "Opened Windows Explorer"
            | OpenCommandLine path ->
                sprintf "Opened Commandline at: %s" path
            | OpenTextEditor name ->
                sprintf "Opened text editor for: %s" name
            | ClipboardCopy path ->
                sprintf "Copied to clipboard: %s" path
            | RemovedNetworkHosts names ->
                let plural = if names.Length > 1 then "s" else ""
                sprintf "Removed network host%s: %s" plural (names |> String.concat ", ")

    type Busy =
        | PuttingItem of isCopy: bool * isRedo: bool * PutItem * PathFormat
        | DeletingItems of permanent: bool * Item list
        | PreparingPut of PutType * name: string
        | CheckingIsRecyclable
        | PreparingDelete of Item list
        | UndoingCreate of Item
        | UndoingPut of isCopy: bool * PutItem * PathFormat
        | RedoingDeleting of permanent: bool * Item list

        member this.Message =
            match this with
            | PuttingItem (isCopy, isRedo, putItem, pathFormat) ->
                let action =
                    if isRedo
                    then sprintf "Redoing %s of" (if isCopy then "copy" else "move")
                    else if isCopy then "Copying" else "Moving"
                sprintf "%s %s..." action ([putItem] |> PutItem.describeList pathFormat)
            | DeletingItems (permanent, items) ->
                let action = if permanent then "Deleting" else "Recycling"
                sprintf "%s %s..." action (Item.describeList items)
            | PreparingPut (putType, name) ->
                sprintf "Preparing to %O %s..." putType name
            | CheckingIsRecyclable ->
                "Determining if items will fit in Recycle Bin..."
            | PreparingDelete items ->
                let descr =
                    match items with
                    | [item] -> item.Description
                    | _ -> sprintf "%i items" items.Length
                sprintf "Preparing to delete %s..." descr
            | UndoingCreate item ->
                sprintf "Undoing creation of %s - Deleting..." item.Description
            | UndoingPut (isCopy, putItem, pathFormat) ->
                let action = if isCopy then "copy" else "move"
                sprintf "Undoing %s of %s..." action (PutItem.describeList pathFormat [putItem])
            | RedoingDeleting (permanent, items) ->
                let action = if permanent then "deletion" else "recycle"
                sprintf "Redoing %s of %s..." action (Item.describeList items)

    type Error =
        | ActionError of actionName: string * exn
        | ItemActionError of ItemAction * PathFormat * exn
        | InvalidPath of string
        | CouldNotOpenPath of Path * PathFormat * exn
        | NoPreviousSearch
        | ShortcutTargetMissing of string
        | PutError of isUndo: bool * PutType * errorPaths: (Path * exn) list * totalItems: int
        | DeleteError of permanent: bool * errorPaths: (Path * exn) list * totalItems: int
        | CannotPutHere
        | CannotUseNameAlreadyExists of actionName: string * itemType: ItemType * name: string * hidden: bool
        | CannotMoveToSameFolder
        | TooManyCopies of fileName: string
        | CouldNotDeleteMoveSource of name: string * exn
        | CouldNotDeleteCopyDest of name: string * exn
        | CannotUndoNonEmptyCreated of Item
        | CannotUndoDelete of permanent: bool * items: Item list
        | CannotRedoPutToExisting of putType: PutType * PutItem * PathFormat
        | NoUndoActions
        | NoRedoActions
        | NoFilesSelected
        | CannotOpenWithMultiple
        | CouldNotOpenFiles of nameErrorPairs: (string * exn) list
        | CouldNotOpenApp of app: string * exn
        | CouldNotFindKoffeeExe

        member private this.ItemErrorsDescription actionName (errorPaths: (Path * exn) list) totalItemCount =
            let actionMsg = "Could not " + actionName
            match errorPaths with
            | [(path, ex)] ->
                sprintf "%s %s: %s" actionMsg path.Name ex.Message
            | (_, ex) :: _ ->
                sprintf "%s %i of %i total items. First error: %s" actionMsg errorPaths.Length totalItemCount ex.Message
            | [] ->
                actionMsg

        member this.Message =
            match this with
            | ActionError (action, e) ->
                let msg =
                    match e with
                    | :? AggregateException as agg -> agg.InnerExceptions.[0].Message
                    | e -> e.Message
                sprintf "Could not %s: %s" action msg
            | ItemActionError (action, pathFormat, e) ->
                (ActionError (action.Description pathFormat, e)).Message
            | InvalidPath path ->
                "Path format is invalid: " + path
            | CouldNotOpenPath (path, pathFormat, ex) ->
                sprintf "Could not open %s: %s" (path.Format pathFormat) ex.Message
            | NoPreviousSearch ->
                "No previous search to repeat"
            | ShortcutTargetMissing path ->
                "Shortcut target does not exist: " + path
            | PutError (isUndo, putType, errorPaths, totalItems) ->
                let undo = if isUndo then "undo " else ""
                this.ItemErrorsDescription (undo + putType.ToLowerString()) errorPaths totalItems
            | DeleteError (permanent, errorPaths, totalItems) ->
                let action = if permanent then "delete"  else "recycle"
                this.ItemErrorsDescription action errorPaths totalItems
            | CannotPutHere ->
                "Cannot put items here"
            | CannotUseNameAlreadyExists (actionName, itemType, name, hidden) ->
                let append = if hidden then " (hidden)" else ""
                sprintf "Cannot %s %O \"%s\" because an item with that name already exists%s"
                        actionName itemType name append
            | CannotMoveToSameFolder ->
                "Cannot move item to same folder it is already in"
            | TooManyCopies fileName ->
                sprintf "There are already too many copies of \"%s\"" fileName
            | CouldNotDeleteMoveSource (name, ex) ->
                sprintf "Could not delete source folder \"%s\" after moving: %s" name ex.Message
            | CouldNotDeleteCopyDest (name, ex) ->
                sprintf "Could not delete copy destination folder \"%s\" after undoing copy: %s" name ex.Message
            | CannotUndoNonEmptyCreated item ->
                sprintf "Cannot undo creation of %s because it is no longer empty" item.Description
            | CannotUndoDelete (permanent, items) ->
                if permanent
                then sprintf "Cannot undo deletion of %s" (Item.describeList items)
                else sprintf "Cannot undo recycling of %s. Please open the Recycle Bin in Windows Explorer to restore items" (Item.describeList items)
            | CannotRedoPutToExisting (putType, putItem, pathFormat) ->
                sprintf "Could not redo %s of %s because an item already exists at %s"
                    (putType.ToLowerString()) putItem.Source.Name (putItem.Dest.Format pathFormat)
            | NoUndoActions ->
                "No more actions to undo"
            | NoRedoActions ->
                "No more actions to redo"
            | NoFilesSelected ->
                "No files selected"
            | CannotOpenWithMultiple ->
                "Cannot use Open File With on multiple items"
            | CouldNotOpenFiles nameErrorPairs ->
                match nameErrorPairs with
                | [name, ex] ->
                    sprintf "Could not open file '%s': %s" name ex.Message
                | (name, ex) :: _ ->
                    sprintf "Could not open %i files. First error: '%s' - %s" nameErrorPairs.Length name ex.Message
                | _ ->
                    "Could not open files"
            | CouldNotOpenApp (app, e) ->
                sprintf "Could not open app %s: %s" app e.Message
            | CouldNotFindKoffeeExe ->
                "Could not determine Koffee.exe path"

    type StatusType =
        | Message of Message
        | Error of Error
        | Busy of Busy

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

type Limits = {
    Back: int
    Undo: int
    PathHistory: int
    SearchHistory: int
    StatusHistory: int
}
with
    static member Default: Limits = {
        Back = 100
        Undo = 50
        PathHistory = 500
        SearchHistory = 50
        StatusHistory = 20
    }

type Config = {
    StartPath: StartPath
    DefaultPath: Path
    PathFormat: PathFormat
    ShowHidden: bool
    TextEditor: string
    CommandlinePath: string
    SearchExclusions: string list
    YankRegister: (ItemRef * PutType) option
    Window: WindowConfig
    Bookmarks: (char * Path) list
    SavedSearches: (char * Search) list
    Limits: Limits
}
with
    static member private addRegister coll item =
        coll
        |> List.filter (fst >> (<>) (fst item))
        |> List.append [item]
        |> List.sortBy (fun (c, _) ->
            // sort an upper case letter immediately after its lower case
            if Char.IsUpper c then Char.ToLower c |> sprintf "%c2" else string c
        )

    member this.GetBookmark char =
        this.Bookmarks |> List.tryFind (fst >> (=) char) |> Option.map snd

    member this.GetSavedSearch char =
        this.SavedSearches |> List.tryFind (fst >> (=) char) |> Option.map snd

    static member withBookmark char path (this: Config) =
        { this with Bookmarks = (char, path) |> Config.addRegister this.Bookmarks }

    static member withoutBookmark char (this: Config) =
        { this with Bookmarks = this.Bookmarks |> List.filter (fst >> (<>) char) }

    static member withSavedSearch char path (this: Config) =
        { this with SavedSearches = (char, path) |> Config.addRegister this.SavedSearches }

    static member withoutSavedSearch char (this: Config) =
        { this with SavedSearches = this.SavedSearches |> List.filter (fst >> (<>) char) }

    static member Default = {
        StartPath = RestorePrevious
        DefaultPath = Path.Root
        PathFormat = Windows
        ShowHidden = false
        TextEditor = "notepad.exe"
        CommandlinePath = "cmd.exe"
        SearchExclusions = [
            ".git"
            ".svn"
            ".hg"
            "CVS"
            ".DS_Store"
            ".fake"
            "bin"
            "obj"
            "packages"
            "node_modules"
        ]
        YankRegister = None
        Window = {
            IsMaximized = false
            Location = (200, 200)
            Size = (800, 800)
            ShowFullPathInTitle = false
            RefreshOnActivate = true
        }
        Bookmarks = []
        SavedSearches = []
        Limits = Limits.Default
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

[<CustomEquality; NoComparison>]
type HistoryPath = {
    PathValue: Path
    IsDirectory: bool
}
with
    member this.Format pathFormat =
        if this.IsDirectory
        then this.PathValue.FormatFolder pathFormat
        else this.PathValue.Format pathFormat

    interface IEquatable<HistoryPath> with
        member this.Equals other = other.PathValue.Equals this.PathValue

    override this.Equals other =
        match other with
        | :? HistoryPath as other -> (this :> IEquatable<HistoryPath>).Equals other
        | _ -> false

    override this.GetHashCode () = this.PathValue.GetHashCode()

    static member Parse str =
        Path.Parse str
        |> Option.map (fun path -> { PathValue = path; IsDirectory = str.EndsWith @"\" || str.EndsWith "/" })

type History = {
    Paths: HistoryPath list
    Searches: Search list
    NetHosts: string list
    PathSort: Map<Path, PathSort>
}
with
    member this.FindSortOrDefault path =
        match this.PathSort.TryFind path with
        | Some sort -> sort
        | None -> PathSort.Default

    static member private pushDistinct max list items =
        (items |> List.rev) @ (list |> List.except items) |> List.truncate max

    static member withSearch searchLimit search (this: History) =
        { this with Searches = History.pushDistinct searchLimit this.Searches [search] }

    static member withoutSearchIndex index (this: History) =
        let before, rest = this.Searches |> List.splitAt index
        { this with Searches = before @ rest.Tail }

    static member private withNetHost host (this: History) =
        if this.NetHosts |> Seq.exists (String.equalsIgnoreCase host) then
            this
        else
            { this with NetHosts = host :: this.NetHosts |> List.sortBy String.toLower }

    static member withoutNetHosts hosts (this: History) =
        let isHostToRemove host = hosts |> List.exists (String.equalsIgnoreCase host)
        { this with NetHosts = this.NetHosts |> List.filter (not << isHostToRemove) }

    static member private withPaths pathLimit isDirectories paths (this: History) =
        if paths |> List.isEmpty then
            this
        else
            let histPaths = paths |> List.map (fun path -> { PathValue = path; IsDirectory = isDirectories })
            { this with Paths = History.pushDistinct pathLimit this.Paths histPaths }

    static member withFilePaths pathLimit paths =
        History.withPaths pathLimit false paths

    static member withFolderPath pathLimit path =
        History.withPaths pathLimit true [path]
        >> Option.foldBack History.withNetHost path.NetHost

    static member withPathReplaced oldPath newPath =
        History.withPathsReplaced (Map [oldPath, newPath])

    static member withPathsReplaced pathMap (this: History) =
        let replaceOld hp =
            match pathMap |> Map.tryPick (fun oldPath newPath -> hp.PathValue.TryReplace oldPath newPath) with
            | Some path -> { hp with PathValue = path }
            | None -> hp
        { this with Paths = this.Paths |> List.map replaceOld |> List.distinct }

    static member withoutPaths (paths: Path list) (this: History) =
        let isPathToRemove historyPath = paths |> List.exists historyPath.PathValue.IsWithin
        { this with Paths = this.Paths |> List.filter (not << isPathToRemove) }

    static member withPathSort path sort (this: History) =
        if sort = PathSort.Default then
            { this with PathSort = this.PathSort.Remove path }
        else
            { this with PathSort = this.PathSort.Add(path, sort) }

    static member Default = {
        Paths = []
        Searches = []
        NetHosts = []
        PathSort = Map.empty
    }

type HistoryDisplayType =
    | NavHistory
    | UndoHistory
    | SearchHistory
    | StatusHistory

type CancelToken() =
    let mutable cancelled = false
    member _.IsCancelled = cancelled
    member _.Cancel () = cancelled <- true

type MainModel = {
    Location: Path
    LocationInput: string
    PathSuggestions: Result<HistoryPath list, string>
    PathSuggestCache: (Path * Result<Path list, string>) option
    Status: MainStatus.StatusType option
    StatusHistory: MainStatus.StatusType list
    Directory: Item list
    Items: Item list
    Sort: (SortField * bool) option
    Cursor: int
    SelectedItems: Item list
    PreviousSelectIndexAndToggle: (int * bool) option
    PageSize: int
    KeyCombo: KeyCombo
    RepeatCommand: int option
    InputMode: InputMode option
    InputText: string
    InputTextSelection: int * int
    InputError: InputError option
    LastFind: string option
    SearchInput: Search
    SearchCurrent: Search option
    SubDirectories: Item list option
    SearchHistoryIndex: int option
    BackStack: (Path * int) list
    ForwardStack: (Path * int) list
    ShowHistoryType: HistoryDisplayType option
    UndoStack: ItemAction list
    RedoStack: ItemAction list
    CancelToken: CancelToken
    WindowLocation: int * int
    WindowSize: int * int
    SaveWindowSettings: bool
    Config: Config
    History: History
} with
    member private this.ClampCursor index =
        index |> clamp 0 (this.Items.Length - 1)

    member this.CursorItem =
        this.Items.[this.Cursor |> this.ClampCursor]

    member this.ActionItems =
        if not this.SelectedItems.IsEmpty then this.SelectedItems else [this.CursorItem]

    member this.PathFormat = this.Config.PathFormat

    member this.LocationFormatted = this.Location.FormatFolder this.PathFormat

    member this.IsSearchingSubFolders = this.SearchCurrent |> Option.exists (fun s -> s.SubFolders)

    member this.TitleLocation =
        if this.Config.Window.ShowFullPathInTitle then
            this.LocationFormatted
        else
            this.Location.Name |> String.ifEmpty this.LocationFormatted

    member this.RepeatCount = this.RepeatCommand |? 1

    member this.ItemsOrEmpty =
        Seq.ifEmpty (Item.EmptyFolder this.SearchCurrent.IsSome this.Location)

    member this.IsStatusBusy =
        match this.Status with
        | Some (MainStatus.Busy _) -> true
        | _ -> false

    member this.IsStatusError =
        match this.Status with
        | Some (MainStatus.Error _) -> true
        | _ -> false

    member this.IsStatusCancelled =
        match this.Status with
        | Some (MainStatus.Message (MainStatus.CancelledPut _))
        | Some (MainStatus.Message (MainStatus.CancelledDelete _)) -> true
        | _ -> false

    static member Default = {
        Location = Path.Root
        LocationInput = Path.Root.Format Windows
        PathSuggestions = Ok []
        PathSuggestCache = None
        Status = None
        StatusHistory = []
        Directory = []
        Items = [ Item.Empty ]
        Sort = Some (Name, false)
        Cursor = 0
        SelectedItems = []
        PreviousSelectIndexAndToggle = None
        PageSize = 30
        KeyCombo = []
        RepeatCommand = None
        InputMode = None
        InputText = ""
        InputTextSelection = 0, 0
        InputError = None
        LastFind = None
        SearchInput = Search.Default
        SearchCurrent = None
        SubDirectories = None
        SearchHistoryIndex = None
        BackStack = []
        ForwardStack = []
        ShowHistoryType = None
        UndoStack = []
        RedoStack = []
        CancelToken = CancelToken()
        WindowLocation = 0, 0
        WindowSize = 800, 800
        SaveWindowSettings = true
        Config = Config.Default
        History = History.Default
    }

    static member withLocation path (this: MainModel) =
        { this with Location = path; LocationInput = path.FormatFolder this.PathFormat }

    static member withPushedLocation path (this: MainModel) =
        if path <> this.Location then
            { this with
                BackStack = (this.Location, this.Cursor) :: this.BackStack |> List.truncate this.Config.Limits.Back
                ForwardStack = []
                Cursor = 0
            }
            |> MainModel.withLocation path
        else this

    static member withCursor index (this: MainModel) =
        { this with Cursor = index |> this.ClampCursor }

    static member withCursorRel move (this: MainModel) =
        MainModel.withCursor (this.Cursor + move) this

    static member selectItems pathsToSelect (this: MainModel) =
        let selected =
            if pathsToSelect |> Seq.isEmpty then
                []
            else
                let itemMap = this.Items |> Seq.map (fun item -> (item.Path, item)) |> Map
                pathsToSelect |> Seq.choose itemMap.TryFind |> Seq.toList
        { this with SelectedItems = selected }

    static member clearSelection (this: MainModel) =
        { this with SelectedItems = []; PreviousSelectIndexAndToggle = None }

    static member private mergeActionsWithSameIntent (actionStack: ItemAction list) =
        match actionStack with
        | PutItems (putType1, intent1, actual1, cancelled1) ::
          PutItems (putType2, intent2, actual2, true) :: tail
                when putType1 = putType2 && PutItem.intentEquals intent1 intent2 ->
            // take distinct items by path, keeping the newer items
            let mergedActual = actual2 @ actual1 |> List.rev |> List.distinctBy (fun pi -> pi.Source) |> List.rev
            PutItems (putType1, intent2, mergedActual, cancelled1) :: tail
        | DeletedItems (permanent1, items1, cancelled1) ::
          DeletedItems (permanent2, items2, true) :: tail
                when permanent1 = permanent2 && items1.Head.Path.Parent = items2.Head.Path.Parent ->
            DeletedItems (permanent1, items2 @ items1, cancelled1) :: tail
        | _ ->
            actionStack

    static member pushUndo action (this: MainModel) =
        let undoStack =
            action :: this.UndoStack
            |> MainModel.mergeActionsWithSameIntent
            |> List.truncate this.Config.Limits.Undo
        { this with UndoStack = undoStack }

    static member withRedoStack stack (this: MainModel) =
        { this with RedoStack = stack }

    static member pushRedo action (this: MainModel) =
        { this with RedoStack = action :: this.RedoStack |> MainModel.mergeActionsWithSameIntent }

    static member withNewCancelToken (this: MainModel) = { this with CancelToken = CancelToken() }

    static member withoutKeyCombo (this: MainModel) = { this with KeyCombo = []; RepeatCommand = None }

    static member appendRepeatDigit digit (this: MainModel) =
        match this.RepeatCommand with
        | None when digit = 0 -> this
        | Some count -> { this with RepeatCommand = Some (count * 10 + digit) }
        | None -> { this with RepeatCommand = Some digit }

    static member withStatus status (this: MainModel) =
        { this with
            Status = Some status
            StatusHistory =
                match status with
                | MainStatus.Busy _ -> this.StatusHistory
                | s -> s :: this.StatusHistory |> List.truncate this.Config.Limits.StatusHistory
        }

    static member withMessage = MainStatus.Message >> MainModel.withStatus
    static member withBusy = MainStatus.Busy >> MainModel.withStatus
    static member withError = MainStatus.Error >> MainModel.withStatus

    static member withResult res (this: MainModel) =
        match res with
        | Ok () -> this
        | Error e -> this |> MainModel.withError e

    static member clearStatus (this: MainModel) =
        { this with Status = None; InputError = None }

    static member mapConfig f (this: MainModel) =
        { this with Config = f this.Config }

    static member mapHistory f (this: MainModel) =
        { this with History = f this.History }

module DragDropEffects =
    let toPutTypes effects =
        if effects = DragDropEffects.All then
            [Move; Copy; Shortcut]
        else
            [
                if effects.HasFlag DragDropEffects.Move then Move
                if effects.HasFlag DragDropEffects.Copy then Copy
                if effects.HasFlag DragDropEffects.Link then Shortcut
            ]

    let ofPutTypes putType =
        match putType with
        | Some Move -> DragDropEffects.Move
        | Some Copy -> DragDropEffects.Copy
        | Some Shortcut -> DragDropEffects.Link
        | None -> DragDropEffects.None

    let fromKeyModifiers () =
        let ctrl = Keyboard.Modifiers.HasFlag(ModifierKeys.Control)
        let shift = Keyboard.Modifiers.HasFlag(ModifierKeys.Shift)
        let alt = Keyboard.Modifiers.HasFlag(ModifierKeys.Alt)
        if ctrl && shift || alt then DragDropEffects.Link
        else if ctrl then DragDropEffects.Copy
        else if shift then DragDropEffects.Move
        else DragDropEffects.None

type DragEvent(event: DragEventArgs) =
    do
        event.Effects <- DragDropEffects.fromKeyModifiers ()

    member this.PutType
        with get () = event.Effects |> DragDropEffects.toPutTypes |> Seq.tryHead
        and set value = event.Effects <- DragDropEffects.ofPutTypes value

    member this.AllowedPutTypes = event.AllowedEffects |> DragDropEffects.toPutTypes

type ScrollType =
    | CursorTop
    | CursorMiddle
    | CursorBottom

type MainEvents =
    | KeyPress of (ModifierKeys * Key) * EvtHandler
    | CursorUp
    | CursorDown
    | CursorUpHalfPage
    | CursorDownHalfPage
    | CursorToFirst
    | CursorToLast
    | SelectToggle
    | SelectRange
    | SelectAll
    | Scroll of ScrollType
    | OpenPath of string * EvtHandler
    | OpenSelected
    | OpenParent
    | OpenRoot
    | OpenDefault
    | Back
    | Forward
    | Refresh
    | DeletePathSuggestion of HistoryPath
    | ShowHistory of HistoryDisplayType
    | StartPrompt of PromptType
    | StartConfirm of ConfirmType
    | StartInput of InputType
    | InputCharTyped of char * EvtHandler
    | InputChanged
    | InputBack
    | InputForward
    | InputDelete of isShifted: bool * EvtHandler
    | SubDirectoryResults of Item list
    | UpdateDropInPutType of Path list * DragEvent
    | DropIn of Path list * DragEvent
    | DropOut of PutType
    | SubmitInput
    | CancelInput
    | FindNext
    | RepeatPreviousSearch
    | StartPut of PutType
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
        SelectToggle, "Select/Unselect Item Under Cursor"
        SelectRange, "Select/Unselect Range (Between Cursor and Last Selection)"
        SelectAll, "Select All"
        Scroll CursorTop, "Scroll View to Put Cursor at the Top"
        Scroll CursorMiddle, "Scroll View to Put Cursor at the Middle"
        Scroll CursorBottom, "Scroll View to Put Cursor at the Bottom"
        OpenParent, "Open Parent Folder"
        OpenRoot, "Open Root Directory"
        OpenDefault, "Open Default Path"
        Back, "Back in Navigation History"
        Forward, "Forward in Navigation History"
        Refresh, "Refresh Current Folder"
        SortList Name, "Sort by Name"
        SortList Modified, "Sort by Modified"
        SortList Size, "Sort by Size"
        ToggleHidden, "Show/Hide Hidden Folders and Files"
        StartInput CreateFile, "Create File"
        StartInput CreateFolder, "Create Folder"
        StartInput (Find false), "Find Item Starting With..."
        StartInput (Find true), "Find Item Starting With... (Multi)"
        FindNext, "Go To Next Find Match"
        StartInput Search, "Search For Items"
        RepeatPreviousSearch, "Repeat Previous Search"
        StartPrompt GoToBookmark, "Go To Bookmark"
        StartPrompt GoToSavedSearch, "Go To Saved Search"
        StartPrompt SetBookmark, "Set Bookmark/Saved Search"
        OpenSelected, "Open Selected Item"
        OpenFileWith, "Open File With..."
        OpenFileAndExit, "Open File and Exit"
        StartInput (Rename Begin), "Rename Item (Prepend)"
        StartInput (Rename EndName), "Rename Item (Append to Name)"
        StartInput (Rename End), "Rename Item (Append to Extension)"
        StartInput (Rename ReplaceName), "Rename Item (Replace Name)"
        StartInput (Rename ReplaceAll), "Rename Item (Replace Full Name)"
        StartPut Move, "Start Move Item"
        StartPut Copy, "Start Copy Item"
        StartPut Shortcut, "Start Create Shortcut to Item"
        ClearYank, "Clear Yank Register"
        Put, "Put Item to Move/Copy in Current Folder"
        ClipCopy, "Copy to Clipboard"
        Recycle, "Send to Recycle Bin"
        StartConfirm Delete, "Delete Permanently"
        OpenProperties, "Open Properties"
        OpenWithTextEditor, "Open Selected File With Text Editor"
        OpenExplorer, "Open Windows Explorer at Current Location"
        OpenCommandLine, "Open Windows Commandline at Current Location"
        Undo, "Undo Action"
        Redo, "Redo Action"
        ShowHistory NavHistory, "Show Navigation History"
        ShowHistory UndoHistory, "Show Undo/Redo History"
        ShowHistory StatusHistory, "Show Status History"
        OpenSplitScreenWindow, "Open New Window for Split Screen"
        OpenSettings, "Open Help/Settings"
        Exit, "Exit"
    ]

type Progress(evt: Event<float option>) =
    member _.Start () =
        evt.Trigger (Some 0.0)

    member _.GetIncrementer (goalCount: int) =
        let increment = 1.0 / float goalCount
        fun _ -> evt.Trigger (Some increment)

    member _.Add progressAmount =
        evt.Trigger (Some progressAmount)

    member _.Finish _ =
        evt.Trigger None
