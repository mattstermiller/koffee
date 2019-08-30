module Koffee.ConfigTests

open NUnit.Framework
open FsUnitTyped
open Newtonsoft.Json

[<Test>]
let ``Config can be serialized and deserialized`` () =
    let config =
        { Config.Default with
            DefaultPath = createPath "/c/Documents and Settings/SomeUser/"
            PathFormat = Unix
            YankRegister = Some (createPath "/c/users/name", Folder, Move)
            Bookmarks = [('a', createPath "/c/path1"); ('b', createPath "/c/path2")]
        }
    let converters = FSharpJsonConverters.getAll ()
    let serialized = JsonConvert.SerializeObject(config, Formatting.Indented, converters)
    printfn "%s" serialized
    let deserialized = JsonConvert.DeserializeObject<Config>(serialized, converters)
    deserialized |> shouldEqual config
