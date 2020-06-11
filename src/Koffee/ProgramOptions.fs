namespace Koffee

open System.Text.RegularExpressions
open Acadian.FSharp

type StartOptions = {
    StartPath: string option
    StartLocation: (int * int) option
    StartSize: (int * int) option
}

module ProgramOptions =
    let private (|Path|_|) (arg: string) = if not <| arg.StartsWith("-") then Some arg else None

    let private (|IntTuple|_|) name arg =
        let parse = System.Int32.Parse
        match Regex.Match(arg, sprintf @"--%s=(\d+),(\d+)" name, RegexOptions.IgnoreCase) with
        | m when m.Success -> Some (parse m.Groups.[1].Value, parse m.Groups.[2].Value)
        | _ -> None

    let parseArgs (args: string list) =
        let args = args |> List.map (String.trimChars [|'"'|])
        let rec parse args options =
            match args with
            | Path path :: rest when options.StartPath.IsNone ->
                parse rest { options with StartPath = Some (path |> IOPath.GetFullPath) }
            | IntTuple "location" loc :: rest when options.StartLocation.IsNone ->
                parse rest { options with StartLocation = Some loc }
            | IntTuple "size" loc :: rest when options.StartSize.IsNone ->
                parse rest { options with StartSize = Some loc }
            | _ :: rest -> parse rest options
            | [] -> options
        parse args { StartPath = None; StartLocation = None; StartSize = None }
