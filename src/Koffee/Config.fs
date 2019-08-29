namespace Koffee

open System
open System.IO
open FSharp.Reflection
open Acadian.FSharp
open Newtonsoft.Json
open Koffee

type StartPath =
    | RestorePrevious
    | DefaultPath

type WindowConfig = {
    IsMaximized: bool
    Left: int
    Top: int
    Width: int
    Height: int
    ShowFullPathInTitle: bool
    RefreshOnActivate: bool
}

type Config = {
    StartPath: StartPath
    DefaultPath: string
    PreviousPath: string
    PathFormat: PathFormat
    ShowHidden: bool
    SearchCaseSensitive: bool
    TextEditor: string
    CommandlinePath: string
    YankRegister: (Path * NodeType * PutAction) option
    Window: WindowConfig
    Bookmarks: (char * Path) list
    NetHosts: string list
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

    member this.WithNetHost host =
        if this.NetHosts |> Seq.exists (String.equalsIgnoreCase host) then
            this
        else
            { this with NetHosts = host :: this.NetHosts |> List.sortBy String.toLower }

    static member Default = {
        StartPath = RestorePrevious
        DefaultPath = Path.Root.Format Windows
        PreviousPath = Path.Root.Format Windows
        PathFormat = Windows
        ShowHidden = false
        SearchCaseSensitive = false
        TextEditor = "notepad.exe"
        CommandlinePath = "cmd.exe"
        YankRegister = None
        Window = {
            IsMaximized = false
            Left = 200
            Top = 200
            Width = 800
            Height = 800
            ShowFullPathInTitle = false
            RefreshOnActivate = true
        }
        Bookmarks = []
        NetHosts = []
    }

module FSharpJsonConverters =
    /// Serializes unions with no fields as just the case name
    type UnionJsonConverter() =
        inherit JsonConverter() with
            override this.CanConvert typ =
                FSharpType.IsUnion typ &&
                FSharpType.GetUnionCases typ |> Seq.forall (fun c -> c.GetFields() |> Array.isEmpty)

            override this.ReadJson (reader, typ, _, _) =
                let caseName = reader.Value :?> string
                let case = FSharpType.GetUnionCases(typ) |> Seq.find (fun c -> c.Name = caseName)
                FSharpValue.MakeUnion(case, [||])

            override this.WriteJson (writer, value, _) =
                sprintf "%A" value |> writer.WriteValue

    /// Serializes option's 'Some' as just the wrapped value
    type OptionJsonConverter() =
        inherit JsonConverter() with
            override this.CanConvert typ =
                typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<_ option>

            override this.ReadJson (reader, typ, _, serializer) =
                if reader.TokenType = JsonToken.Null then
                    null
                else
                    let innerType = typ.GetGenericArguments().[0]
                    let value = serializer.Deserialize(reader, innerType)
                    let some = FSharpType.GetUnionCases(typ) |> Seq.find (fun c -> c.Name = "Some")
                    FSharpValue.MakeUnion(some, [|value|])

            override this.WriteJson (writer, value, serializer) =
                let innerValue =
                    if value = null then
                        null
                    else
                        let _, fields = FSharpValue.GetUnionFields(value, value.GetType())
                        fields.[0]
                serializer.Serialize(writer, innerValue)

    type PathJsonConverter() =
        inherit JsonConverter() with
            override this.CanConvert typ = typ = typeof<Path>

            override this.ReadJson (reader, _, _, _) =
                reader.Value :?> string |> Path.Parse |> Option.get |> box

            override this.WriteJson (writer, value, _) =
                (value :?> Path).Format Windows |> writer.WriteValue

    let getAll () : JsonConverter[] = [|
        UnionJsonConverter()
        OptionJsonConverter()
        PathJsonConverter()
    |]

type PersistFile<'a when 'a : equality>(filePath: string, defaultValue: 'a) =
    let mutable value = defaultValue
    let mutable watcher = new FileSystemWatcher(IO.Path.GetDirectoryName filePath,
                                                IO.Path.GetFileName filePath)
    let fileChangedEvent = Event<'a>()

    let converters = FSharpJsonConverters.getAll ()
    let serialize a = JsonConvert.SerializeObject(a, Formatting.Indented, converters)
    let deserialize text = JsonConvert.DeserializeObject<'a>(text, converters)

    let defaultNullProps o =
        let isOption (t: Type) = t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<_ option>
        let rec inner def o =
            let fields = FSharpType.GetRecordFields(o.GetType())
            let values = fields |> Array.map (fun field ->
                let value = field.GetValue o
                if value = null && not (isOption field.PropertyType) then
                    (field.GetValue def, true)
                else if FSharpType.IsRecord field.PropertyType then
                    inner (field.GetValue def) (field.GetValue o)
                else
                    (value, false)
            )
            if values |> Array.exists snd then
                (FSharpValue.MakeRecord(o.GetType(), values |> Array.map fst), true)
            else
                (o, false)
        (inner defaultValue o |> fst) :?> 'a

    let load () =
        try
            value <- File.ReadAllText(filePath) |> deserialize |> defaultNullProps
        with _ ->
            try
                File.Copy(filePath, filePath + ".invalid")
            with _ -> ()

    let save () =
        watcher.EnableRaisingEvents <- false
        File.WriteAllText(filePath, serialize value)
        watcher.EnableRaisingEvents <- true

    do
        if File.Exists filePath then
            load ()
        else
            let dir = Path.GetDirectoryName filePath
            if not <| Directory.Exists dir then
                Directory.CreateDirectory dir |> ignore
            save ()
        watcher.Changed.Add(fun _ ->
            load ()
            fileChangedEvent.Trigger value
        )
        watcher.EnableRaisingEvents <- true

    member this.Value
        with get () = value
        and set v =
            if value <> v then
                value <- v
                save ()

    member this.FileChanged = fileChangedEvent.Publish

    interface IDisposable with
        member this.Dispose () = watcher.Dispose()

type ConfigFile(defaultValue) =
    inherit PersistFile<Config>(ConfigFile.FilePath, defaultValue)
    static member FilePath = Path.KoffeeData.Join("config.json").Format Windows
