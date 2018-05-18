namespace Koffee

open System
open System.Text.RegularExpressions
open Utility

type IOPath = System.IO.Path

type PathFormat =
    | Windows
    | Unix
    override this.ToString() = sprintf "%A" this

type Path private (path: string) =
    static let root = ""
    static let rootWindows = "Drives"
    static let rootUnix = "/"
    static let net = "Network"
    static let netUnix = "/net/"

    static let invalidChars = (IOPath.GetInvalidPathChars() |> String) + "?*"

    static let matchPathWithPrefix prefixPattern (prefixMapping: string -> string) =
        let pattern = sprintf @"^(%s)([/\\][^%s]*)?$" prefixPattern invalidChars
        let re = Regex(pattern, RegexOptions.Compiled ||| RegexOptions.IgnoreCase)
        (fun s ->
            let m = re.Match(s)
            if m.Success then
                let prefix = m.Groups.[1].Value |> prefixMapping
                let dirs = m.Groups.[2].Value.Replace("/", @"\").Trim('\\')
                let join =
                    if not <| prefix.EndsWith(@"\") && dirs <> "" then @"\"
                    else ""
                Some (Path (prefix + join + dirs))
            else None)

    static let (|RootPath|_|) s =
        if Seq.exists (Str.equalsIgnoreCase s) [root; rootUnix; rootWindows] then
            Some (Path root)
        else None

    static let (|LocalPath|_|) =
        matchPathWithPrefix "[a-z]:|/[a-z]" (fun drive -> drive.Trim(':', '/', '\\').ToUpper() + @":\")

    static let (|UserPath|_|) =
        matchPathWithPrefix "~" (fun _ -> Path.UserDirectory)

    static let (|NetRootPath|_|) s =
        if Seq.exists (Str.equalsIgnoreCase s) [net; netUnix; netUnix.TrimEnd('/')] then
            Some (Path net)
        else None

    static let (|NetPath|_|) =
        let hostChars = "a-z0-9-_."
        let pattern = sprintf @"[/\\]{2}[%s]+|/net/[%s]+" hostChars hostChars
        matchPathWithPrefix pattern (fun prefix ->
            let server = prefix.Replace("/net/", "").Trim('/', '\\')
            sprintf @"\\%s" server)

    static member Root = Path root
    static member Network = Path net
    static member UserDirectory = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
    static member KoffeeData = Path(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData)).Join "Koffee"

    static member Parse (s: string) =
        match s.Trim() with
        | RootPath p
        | LocalPath p
        | UserPath p
        | NetRootPath p
        | NetPath p -> Some p
        | _ -> None

    static member SplitName (name: string) =
        match name.LastIndexOf('.') with
        | -1 -> (name, "")
        | index -> (name.Substring(0, index), name.Substring(index))


    member private this.Value = path

    member this.Format fmt =
        match fmt with
        | Windows ->
            if path = root then rootWindows
            else path
        | Unix ->
            if path = root then
                rootUnix
            else if path = net then
                netUnix
            else if this.IsNetPath then
                path.Insert(1, "net").Replace(@"\", "/")
            else
                let drive = (string path.[0]).ToLower()
                let dirs = path.Substring(2).Replace(@"\", "/")
                sprintf "/%s%s" drive dirs

    member this.Name =
        path |> IOPath.GetFileName

    member this.NameAndExt = Path.SplitName this.Name

    member this.Drive =
        match path with
        | LocalPath _ -> IOPath.GetPathRoot(path) |> Path |> Some
        | _ -> None

    member this.NetHost =
        if this.IsNetPath then Some <| path.Substring(2).Split('\\').[0]
        else None

    member this.Parent =
        if path = root || path = net then
            Path.Root
        else if this.IsNetHost then
            Path.Network
        else if this.IsNetPath then
            path.Substring(0, path.LastIndexOf('\\')) |> Path
        else
            IOPath.GetDirectoryName path
            |> Option.ofObj
            |> Option.defaultValue root
            |> Path

    member this.Join name =
        IOPath.Combine(path, name) |> Path

    member this.IsNetPath = path.StartsWith(@"\\")

    member this.IsNetHost =
        this.IsNetPath && path.Length > 2 && not (path.Substring(2).Contains(@"\"))

    override this.Equals other =
        match other with
        | :? Path as p -> this.Value = p.Value
        | _ -> false

    override this.GetHashCode() = this.Value.GetHashCode()

    // for debugging
    override this.ToString() = this.Format Windows
