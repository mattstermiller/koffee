module Koffee.EnvUtility

open System.Text.RegularExpressions

let private subRegex = new Regex(@"%([^%]*)%", RegexOptions.Compiled)

/// Substitutes %variables% in string using IOperatingSystem.GetEnvironmentVariable
///
/// Example input: "Hello %target%!"
/// Outputs: "Hello world!"
/// (where "target" variable is set to "world")
let subEnvVars (subs: IOperatingSystem) str =
    let mayContainSubstitution (str: string) =
        str.IndexOf '%' <> -1

    let matchEval (m: Match) =
        subs.GetEnvironmentVariable m.Groups.[1].Value
        |> Option.defaultValue m.Value

    if mayContainSubstitution str then
        subRegex.Replace(str, matchEval)
    else
        str

