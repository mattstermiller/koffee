namespace Koffee

open System
open System.ComponentModel
open System.Text.RegularExpressions
open Acadian.FSharp

type IOPath = System.IO.Path

type PathFormat =
    | Windows
    | Unix
    override this.ToString() = sprintf "%A" this
    member this.Separator =
        match this with
        | Windows -> @"\"
        | Unix -> "/"

[<TypeConverter(typeof<PathToStringTypeConverter>)>]
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
            else None
        )

    static let (|RootPath|_|) s =
        if Seq.exists (String.equalsIgnoreCase s) [root; rootUnix; rootWindows] then
            Some (Path root)
        else None

    static let (|LocalPath|_|) =
        matchPathWithPrefix "[a-z]:|/[a-z]" (fun drive -> drive.Trim(':', '/', '\\').ToUpper() + @":\")

    static let (|UserPath|_|) =
        matchPathWithPrefix "~" (fun _ -> Path.UserDirectory)

    static let (|NetRootPath|_|) s =
        if Seq.exists (String.equalsIgnoreCase s) [net; netUnix; netUnix.TrimEnd('/')] then
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

    static member InvalidNameChars = invalidChars + ":/\\"

    static member GetTimestamp () = System.DateTime.Now.ToString("yyyy-MM-dd_HH-mm-ss")

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

    member this.FormatFolder fmt =
        let formatted = this.Format fmt
        if path = root || path = net || formatted |> String.endsWith fmt.Separator then
            formatted
        else
            formatted + fmt.Separator

    member this.FormatRelativeFolder fmt (relativeTo: Path) =
        if this = Path.Root then ""
        else (this.Format fmt |> String.replace (relativeTo.Format fmt) ".") + fmt.Separator

    member val Name =
        if path.Length = 3 && path.EndsWith @":\"
        then path.Substring(0, 2)
        else path |> IOPath.GetFileName

    member this.Extension =
        path |> IOPath.GetExtension |> String.trimStart [|'.'|]

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
            |? root
            |> Path

    member this.Base =
        if path = root || path = net then
            this
        else
            IOPath.GetPathRoot(path) |> Path

    member this.Join name =
        if path = root then
            Path.Parse name
            |> Option.orElseWith (fun () ->
                Path.Parse ("/" + name)
            )
            |? Path.Root
        else if path = net then
            @"\\" + name |> Path.Parse |? Path.Network
        else
            IOPath.Combine(path, name) |> Path

    member this.IsWithin (otherPath: Path) =
        if otherPath = Path.Root || otherPath = this
        then true
        else path |> String.startsWithIgnoreCase (otherPath.FormatFolder Windows)

    member this.TryReplace (oldPath: Path) (newPath: Path) =
        if oldPath <> Path.Root && this.IsWithin oldPath then
            let relative = path |> String.replace (string oldPath) "" |> String.trimStart [|'\\'|]
            Some (newPath.Join relative)
        else
            None

    member this.IsNetPath = path.StartsWith(@"\\")

    member this.IsNetHost =
        this.IsNetPath && path.Length > 2 && not (path.Substring(2).Contains(@"\"))

    // needed to use Path as Map key
    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? Path as p -> String.Compare(this.Value, p.Value, ignoreCase=true)
            | _ -> 0

    override this.Equals other =
        match other with
        | :? Path as p -> this.Value |> String.equalsIgnoreCase p.Value
        | _ -> false

    override this.GetHashCode() = this.Value.ToLower().GetHashCode()

    override this.ToString() = this.Format Windows

// allows using Path as the key of Map/dictionary in Json.NET
and private PathToStringTypeConverter() =
    inherit TypeConverter()
    override _.CanConvertFrom (_, typ) = typ = typeof<string>
    override _.CanConvertTo (_, typ) = typ = typeof<string>

    override _.ConvertTo(context, culture, value, destinationType) =
        if destinationType = typeof<string> then
            (value :?> Path).Format Windows |> box
        else
            base.ConvertTo(context, culture, value, destinationType)

    override _.ConvertFrom(context, culture, value) =
        match value with
        | :? string as stringValue -> stringValue |> Path.Parse |> Option.get |> box
        | _ -> base.ConvertFrom(context, culture, value)
