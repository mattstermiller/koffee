namespace Koffee

open System
open System.IO
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

    /// Serializes unions with no fields as just the case name and unions with one field as the case name, space, argument
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
                let case = FSharpType.GetUnionCases(typ) |> Seq.find (fun c -> c.Name = caseName)
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
                FSharpValue.MakeUnion(case, value |> Option.toArray)

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
            override this.CanConvert typ = typ = typeof<ModifierKeys * Key>

            override this.ReadJson (reader, _, _, _) =
                reader.Value :?> string
                |> KeyBindingLogic.Serialization.parseChord
                |? (ModifierKeys.None, Key.None)
                |> box

            override this.WriteJson (writer, value, _) =
                (value :?> ModifierKeys * Key)
                |> KeyBindingLogic.Serialization.chordString
                |> writer.WriteValue

    let getAll () : JsonConverter[] = [|
        OptionJsonConverter()
        UnionJsonConverter()
        PathJsonConverter()
        HistoryPathJsonConverter()
        KeyChordConverter()
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

type HistoryFile(defaultValue) as this =
    inherit PersistFile<History>(HistoryFile.FilePath, defaultValue)

    do
        // filter out corrupt search entries loaded from previous version formats
        this.Value <- { this.Value with Searches = this.Value.Searches |> List.filter (fun s -> s.Terms <> null) }

    static member FilePath = Path.KoffeeData.Join("history.json").Format Windows
