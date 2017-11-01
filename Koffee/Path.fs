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

    static let firstModify f (s: string) =
        if String.IsNullOrEmpty(s) then ""
        else
            let first = string s.[0] |> f
            if s.Length > 1 then first + s.Substring(1)
            else first
    static let firstToUpper = firstModify (fun s -> s.ToUpper())
    static let firstToLower = firstModify (fun s -> s.ToLower())

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

    static let (|NetPath|_|) =
        let hostChars = "a-z0-9-_."
        let pattern = sprintf @"[/\\]{2}[%s]+|/net/[%s]+" hostChars hostChars
        matchPathWithPrefix pattern (fun prefix ->
            let server = prefix.Replace("/net/", "").Trim('/', '\\')
            sprintf @"\\%s" server)

    static member Root = Path root
    static member UserDirectory = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile);

    static member Parse (s: string) =
        match s.Trim() with
        | RootPath p
        | LocalPath p
        | UserPath p
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
            else if this.IsNetPath then
                path.Insert(1, "net").Replace(@"\", "/")
            else
                (path |> firstToLower).Replace(":", "").Insert(0, "/").Replace(@"\", "/")

    member this.Name =
        path |> IOPath.GetFileName

    member this.NameAndExt = Path.SplitName this.Name

    member this.Drive =
        match path with
        | p when p.Length > 0 && Char.IsLetter p.[0] -> Some (string p.[0])
        | _ -> None

    member this.Parent =
        if path = root then
            Path.Root
        else
            IOPath.GetDirectoryName path
            |> Option.ofObj
            |> Option.defaultValue root
            |> Path

    member this.Join name =
        IOPath.Combine(path, name) |> Path

    member this.IsNetPath = path.StartsWith(@"\\")

    override this.Equals other =
        match other with
        | :? Path as p -> this.Value = p.Value
        | _ -> false

    override this.GetHashCode() = this.Value.GetHashCode()

    // for debugging
    override this.ToString() = this.Format Windows
