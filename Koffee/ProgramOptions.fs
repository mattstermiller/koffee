namespace Koffee

open System.Text.RegularExpressions

type StartupOptions = {
    StartupPath: string option
    Location: (int * int) option
    Size: (int * int) option
}

module ProgramOptions =
    let private (|Path|_|) (arg: string) = if not <| arg.StartsWith("-") then Some arg else None

    let private (|IntTuple|_|) name arg =
        let parse = System.Int32.Parse
        match Regex.Match(arg, sprintf @"--%s=(\d+),(\d+)" name, RegexOptions.IgnoreCase) with
        | m when m.Success -> Some (parse m.Groups.[1].Value, parse m.Groups.[2].Value)
        | _ -> None

    let parseArgs (args: string list) =
        let rec parse args options =
            match args with
            | Path path :: rest when options.StartupPath.IsNone ->
                parse rest { options with StartupPath = Some path }
            | IntTuple "location" loc :: rest when options.Location.IsNone ->
                parse rest { options with Location = Some loc }
            | IntTuple "size" loc :: rest when options.Size.IsNone ->
                parse rest { options with Size = Some loc }
            | _ :: rest -> parse rest options
            | [] -> options
        parse args { StartupPath = None; Location = None; Size = None }
