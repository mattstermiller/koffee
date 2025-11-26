module Koffee.PersistenceTests

open System
open System.Text.RegularExpressions
open NUnit.Framework
open FsUnitTyped
open Newtonsoft.Json
open Acadian.FSharp

let converters = FSharpJsonConverters.getAll ()

let serialize a =
    JsonConvert.SerializeObject(a, Formatting.Indented, converters)

let deserialize<'a> json =
    JsonConvert.DeserializeObject<'a>(json, converters)

let sanitize =
    Persistence.defaultNullProps Config.Default
    >> ConfigFile.Sanitize

let testConfig =
    { Config.Default with
        DefaultPath = createPath "/c/Documents and Settings/SomeUser/"
        PathFormat = Unix
        YankRegister = Some (Move, [{ Path = createPath "/c/users/name"; Type = Folder }])
        Bookmarks = [('a', createPath "/c/path1"); ('b', createPath "/c/path2")]
    }

[<Test>]
let ``Config can be serialized and deserialized`` () =
    let serialized = serialize testConfig
    deserialize<Config> serialized
    |> shouldEqual testConfig

[<Test>]
let ``Config with missing property and invalid union value deserializes with defaults`` () =
    let mangled =
        serialize testConfig
        |> fun s -> Regex.Replace(s, "\"StartPath\": .*", String.Empty)
        |> fun s -> Regex.Replace(s, "\"Unix\"", "\"InvalidPathFormat\"")
    deserialize<Config> mangled |> sanitize
    |> assertAreEqual
        { testConfig with
            StartPath = Config.Default.StartPath
            PathFormat = Config.Default.PathFormat
        }

[<Test>]
let ``Converters serialize and deserialize desired format for key bindings`` () =
    let expectedJson = String.trim """
[
  {
    "KeyCombo": [
      "Oem1"
    ],
    "Command": "Cursor FindNext"
  },
  {
    "KeyCombo": [
      "Control+Shift+S",
      "T"
    ],
    "Command": "Cursor Scroll CursorTop"
  }
]
"""
    let keyBindings =
        [
            [ModifierKeys.None, Key.Oem1], Cursor FindNext
            [
                ModifierKeys.Control ||| ModifierKeys.Shift, Key.S
                ModifierKeys.None, Key.T
            ], Cursor (Scroll CursorTop)
        ]
        |> List.map KeyBinding.ofTuple

    let serialized = serialize keyBindings
    serialized |> shouldEqual expectedJson

    deserialize<KeyBinding<MainCommand> list> serialized
    |> shouldEqual keyBindings

[<Test>]
let ``KeyBindings with an invalid command can be deserialized without invalid binding`` () =
    let json = String.trim """
{
  "KeyBindings": [
    {
      "KeyCombo": ["J"],
      "Command": "Cursor CursorDown"
    },
    {
      "KeyCombo": ["H"],
      "Command": "Cursor NotACommand"
    },
    {
      "KeyCombo": ["H"],
      "Command": "NotAType NotACommand"
    },
    {
      "KeyCombo": ["K"],
      "Command": "Cursor CursorUp"
    }
  ]
}
"""
    let expectedKeyBindings =
        [
            [ModifierKeys.None, Key.J], Cursor CursorDown
            [ModifierKeys.None, Key.K], Cursor CursorUp
        ]
        |> List.map KeyBinding.ofTuple

    let deserializedConfig = deserialize<Config> json |> sanitize
    deserializedConfig.KeyBindings |> shouldEqual expectedKeyBindings


[<Test>]
let ``runWithRetries recovers after max retry failures`` () =
    let mutable tryNumber = 0
    Persistence.runWithRetries 0 (fun () ->
        tryNumber <- tryNumber + 1
        if tryNumber <= Persistence.maxRetries then
            failwith "failure!"
    )

[<Test>]
let ``runWithRetries throws last exception when exceeding max retries`` () =
    let mutable failure = 0
    try
        Persistence.runWithRetries 0 (fun () ->
            failure <- failure + 1
            failwithf "failure %i!" failure
        )
        failwith "This shouldn't execute"
    with e ->
        e.Message |> shouldEqual (sprintf "failure %i!" (Persistence.maxRetries + 1))
