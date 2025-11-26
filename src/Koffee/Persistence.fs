namespace Koffee

open System
open System.IO
open System.Threading
open System.Windows
open FSharp.Reflection
open Acadian.FSharp
open Newtonsoft.Json
open Koffee

module FSharpJsonConverters =
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

    /// Serializes unions with no fields as just the case name and unions with one field as the case name, space, argument.
    /// Returns null when invalid (to be replaced with a default or filtered out later)
    type UnionJsonConverter() =
        inherit JsonConverter() with
            override this.CanConvert typ =
                FSharpType.IsUnion typ &&
                FSharpType.GetUnionCases typ |> Seq.forall (fun c -> c.GetFields().Length < 2)

            let rec parseUnion (serializer: JsonSerializer) typ (str: string) =
                let (caseName, valueStr) =
                    match str.IndexOf " " with
                    | -1 -> (str, None)
                    | i -> (str.Substring(0, i), Some (str.Substring(i + 1)))
                let cases = FSharpType.GetUnionCases(typ)
                match cases |> Seq.tryFind (fun c -> c.Name = caseName) with
                | Some case ->
                    let valueType = case.GetFields() |> Array.tryHead |> Option.map (fun field -> field.PropertyType)
                    let value =
                        (valueType, valueStr) ||> Option.map2 (fun typ str ->
                            if typ |> FSharpType.IsUnion then
                                parseUnion serializer typ str
                            else
                                use reader = new StringReader(str)
                                use jsonReader = new JsonTextReader(reader)
                                serializer.Deserialize jsonReader
                        )
                    match valueType, value with
                    | Some typ, Some null when not (typ |> Reflection.isGenericType typedefof<option<_>>) ->
                        null // return null when value is an invalid null
                    | _ ->
                        FSharpValue.MakeUnion(case, value |> Option.toArray)
                | None ->
                    null // default to null

            override this.ReadJson (reader, typ, _, serializer) =
                reader.Value :?> string
                |> parseUnion serializer typ

            override this.WriteJson (writer, value, _) =
                string value
                |> String.replace "(" ""
                |> String.replace ")" ""
                |> writer.WriteValue

    type PathJsonConverter() =
        inherit JsonConverter() with
            override this.CanConvert typ = typ = typeof<Path>

            override this.ReadJson (reader, _, _, _) =
                reader.Value :?> string |> Path.Parse |> Option.get |> box

            override this.WriteJson (writer, value, _) =
                (value :?> Path).Format Windows |> writer.WriteValue

    /// Serializes HistoryPaths as normal Path strings except directories have trailing slash
    type HistoryPathJsonConverter() =
        inherit JsonConverter() with
            override this.CanConvert typ = typ = typeof<HistoryPath>

            override this.ReadJson (reader, _, _, _) =
                reader.Value :?> string |> HistoryPath.Parse |> Option.get |> box

            override this.WriteJson (writer, value, _) =
                (value :?> HistoryPath).Format Windows |> writer.WriteValue

    type KeyChordConverter() =
        inherit JsonConverter() with
            override this.CanConvert typ = typ = typeof<KeyChord>

            override this.ReadJson (reader, _, _, _) =
                reader.Value :?> string
                |> KeyChord.deserialize
                |? (ModifierKeys.None, Key.None)
                |> box

            override this.WriteJson (writer, value, _) =
                (value :?> KeyChord)
                |> KeyChord.serialize
                |> writer.WriteValue

    let getAll () : JsonConverter[] = [|
        OptionJsonConverter()
        UnionJsonConverter()
        PathJsonConverter()
        HistoryPathJsonConverter()
        KeyChordConverter()
    |]

module Persistence =
    let maxRetries = 3

    let runWithRetries baseDelayMs action =
        Seq.init (maxRetries + 1) (fun retry ->
            try
                Some (action())
            with _ when retry < maxRetries ->
                if baseDelayMs > 0.0 then
                    Thread.Sleep(baseDelayMs * Math.Pow(2, retry) |> int)
                None
        )
        |> Seq.pick id

    let defaultNullProps (defaultValue: 'a) (obj: 'a) =
        let isOption (t: Type) = t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<_ option>
        let rec inner defaultValue obj =
            let fields = FSharpType.GetRecordFields(obj.GetType())
            let values = fields |> Array.map (fun field ->
                let value = field.GetValue obj
                if value = null && not (isOption field.PropertyType) then
                    (field.GetValue defaultValue, true)
                else if FSharpType.IsRecord field.PropertyType then
                    inner (field.GetValue defaultValue) (field.GetValue obj)
                else
                    (value, false)
            )
            if values |> Array.exists snd then
                (FSharpValue.MakeRecord(obj.GetType(), values |> Array.map fst), true)
            else
                (obj, false)
        (inner defaultValue obj |> fst) :?> 'a

    let logAndShowError isCrash (e: exn) =
        let typ = if isCrash then "crash" else "error"
        let logFilePath = Path.KoffeeData.Join(sprintf "%s_%s.log" typ (Path.GetTimestamp())) |> string
        let logWritten =
            try
                File.WriteAllText(logFilePath, string e)
                sprintf "This error has been logged to: \n%s\n\n" logFilePath
            with _ -> ""
        let msg =
            sprintf "Sorry! An unexpected error %s:\n\n"
                    (if isCrash then "caused Koffee to crash" else "occurred in Koffee") +
            sprintf "%s\n\n" e.Message +
            logWritten +
            "Please report this as an issue on Koffee's GitHub project:\n" +
            "https://github.com/mattstermiller/koffee/issues"
        MessageBox.Show(msg, sprintf "Koffee %s!" typ, MessageBoxButton.OK, MessageBoxImage.Error) |> ignore

type PersistFile<'a when 'a : equality>(filePath: string, defaultValue: 'a, sanitize: 'a -> 'a) =
    let mutable value = defaultValue
    let mutable watcher = new FileSystemWatcher(IO.Path.GetDirectoryName filePath,
                                                IO.Path.GetFileName filePath)
    let fileChangedEvent = Event<'a>()

    let converters = FSharpJsonConverters.getAll ()
    let serialize a = JsonConvert.SerializeObject(a, Formatting.Indented, converters)
    let deserialize text = JsonConvert.DeserializeObject<'a>(text, converters)

    let backUpFile backupType filePath =
        try
            let directory = Path.GetDirectoryName filePath
            let fileName = Path.GetFileNameWithoutExtension filePath
            let ext = Path.GetExtension filePath
            let backupName = String.concat "_" [fileName; backupType; Path.GetTimestamp()] + ext
            File.Copy(filePath, Path.Combine(directory, backupName))
        with _ -> ()

    let runWithRetries action = Persistence.runWithRetries 25.0 action

    let load () =
        // if loading or deserialization fails, take a backup so it doesn't get overwritten
        try
            let fileText = runWithRetries (fun () -> File.ReadAllText(filePath))
            try
                value <- fileText |> deserialize |> Persistence.defaultNullProps defaultValue |> sanitize
            with e ->
                Persistence.logAndShowError false e
                backUpFile "invalid" filePath
        with e ->
            Persistence.logAndShowError false e
            backUpFile "unread" filePath

    let save () =
        let serialized = serialize value
        watcher.EnableRaisingEvents <- false
        try
            runWithRetries (fun () -> File.WriteAllText(filePath, serialized))
        finally
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
    inherit PersistFile<Config>(ConfigFile.FilePath, defaultValue, ConfigFile.Sanitize)

    static member Sanitize config =
        { config with
            KeyBindings = config.KeyBindings |> List.filter (fun kb -> (box kb.Command) <> null)
        }

    static member FilePath = Path.KoffeeData.Join("config.json") |> string

type HistoryFile(defaultValue) =
    inherit PersistFile<History>(HistoryFile.FilePath, defaultValue, HistoryFile.Sanitize)

    static member Sanitize history =
        { history with
            Searches = history.Searches |> List.filter (fun s -> s.Terms <> null)
        }

    static member FilePath = Path.KoffeeData.Join("history.json") |> string
