module Koffee.ConfigTests

open NUnit.Framework
open FsUnitTyped
open Newtonsoft.Json
open Acadian.FSharp

let converters = FSharpJsonConverters.getAll ()

let serialize a = JsonConvert.SerializeObject(a, Formatting.Indented, converters)
let deserialize<'a> json = JsonConvert.DeserializeObject<'a>(json, converters)

[<Test>]
let ``Config can be serialized and deserialized`` () =
    let config =
        { Config.Default with
            DefaultPath = createPath "/c/Documents and Settings/SomeUser/"
            PathFormat = Unix
            YankRegister = Some (Move, [{ Path = createPath "/c/users/name"; Type = Folder }])
            Bookmarks = [('a', createPath "/c/path1"); ('b', createPath "/c/path2")]
        }
    let serialized = serialize config
    let deserialized = deserialize<Config> serialized
    deserialized |> shouldEqual config

[<Test>]
let ``FSharpJsonConverters serializes and deserializes desired format for key bindings`` () =
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

    let deserialized = deserialize<KeyBinding<MainCommand> list> serialized
    deserialized |> shouldEqual keyBindings
