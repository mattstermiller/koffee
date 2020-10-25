module Koffee.EnvUtility

open System.Text.RegularExpressions

let private subRegex = new Regex(@"%([^%]*)%", RegexOptions.Compiled)

let private fastCheckMayContainSub (str: string) =
    str.IndexOf '%' <> -1

/// Substitutes %variables% in string using IOperatingSystem.GetEnvironmentVariable
///
/// Example input: "Hello %TARGET%!"
/// Outputs: "Hello world!"
/// (where "TARGET" variable is set to "world")
let subEnvVars (os: IOperatingSystem) str =
    let matchEval (m: Match) =
        os.GetEnvironmentVariable m.Groups.[1].Value
        |> Option.defaultValue m.Value

    if fastCheckMayContainSub str then
        subRegex.Replace(str, matchEval)
    else
        str

