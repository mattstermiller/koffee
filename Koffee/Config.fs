namespace Koffee

open FSharp.Configuration

type Config = YamlConfig<"Config.yaml">

open System
open System.IO
open Reflection
open Acadian.FSharp

type StartPath =
    | RestorePrevious
    | DefaultPath

[<AutoOpen>]
module ConfigExt =
    let private filePath = Path.KoffeeData.Join("config.yaml").Format Windows

    let private bookmarkKey char = sprintf "_%c" char

    type Config with
        member this.Load () =
            if not <| File.Exists filePath then
                let dir = Path.GetDirectoryName filePath
                if not <| Directory.Exists dir then
                    Directory.CreateDirectory dir |> ignore
                this.Bookmarks.Clear()
                this.NetHosts.Clear()
                File.WriteAllText(filePath, this.ToString())
            this.LoadAndWatch filePath |> ignore

        member this.Save () = this.Save filePath

        member this.PathFormat
            with get () = ParseUnionCase<PathFormat> this.PathFormatName |? Windows
            and set value = this.PathFormatName <- GetUnionCaseName value

        member this.StartPath
            with get () = ParseUnionCase<StartPath> this.StartupPathType |? RestorePrevious
            and set value = this.StartupPathType <- GetUnionCaseName value

        member this.YankRegister
            with get () =
                let path = Koffee.Path.Parse this.YankRegisterPath
                let action = ParseUnionCase<PutAction> this.YankRegisterAction
                (path, action) ||> Option.map2 (fun p a -> p, a)
            and set (value: (Koffee.Path * PutAction) option) =
                let path, action =
                    match value with
                    | Some (path, action) -> path.Format Windows, GetUnionCaseName action
                    | None -> "", ""
                this.YankRegisterPath <- path
                this.YankRegisterAction <- action

        member this.SetBookmark char path =
            this.RemoveBookmark char
            let bookmark = Config.Bookmarks_Item_Type()
            bookmark.Key <- bookmarkKey char
            bookmark.Path <- path
            this.Bookmarks.Add(bookmark)
            this.SortBookmarks()

        member private this.SortBookmarks () =
            let sortKey (bookmark: Config.Bookmarks_Item_Type) =
                let char = bookmark.Key.[1]
                // sort an upper case letter immediately after its lower case
                if Char.IsUpper char then Char.ToLower char |> sprintf "%c2"
                else string char
            this.Bookmarks <- this.Bookmarks |> Seq.sortBy sortKey |> ResizeArray

        member this.RemoveBookmark char =
            let key = bookmarkKey char
            this.Bookmarks
            |> Seq.tryFindIndex (fun b -> b.Key = key)
            |> Option.iter this.Bookmarks.RemoveAt

        member this.GetBookmark char =
            let key = bookmarkKey char
            this.Bookmarks
            |> Seq.tryFind (fun b -> b.Key = key)
            |> Option.map (fun b -> b.Path)

        member this.GetBookmarks () =
            this.Bookmarks
            |> Seq.map (fun b -> b.Key.[1], b.Path)
            |> dict

        member this.AddNetHost host =
            if this.NetHosts |> Seq.exists (String.equalsIgnoreCase host) then
                false
            else
                this.NetHosts.Add host
                this.NetHosts <- this.NetHosts |> Seq.sortBy String.toLower |> ResizeArray
                true

        member this.RemoveNetHost host =
            this.NetHosts
            |> Seq.tryFindIndex (String.equalsIgnoreCase host)
            |> Option.iter this.NetHosts.RemoveAt
