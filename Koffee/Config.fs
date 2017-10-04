namespace Koffee

open FSharp.Configuration

type Config = YamlConfig<"Config.yaml">

open System
open System.IO
open Reflection
open Utility

type StartupPath =
    | RestorePrevious
    | DefaultPath

module ConfigExt =
    let private filePath =
        let appData = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData)
        Path.Combine(appData, "Koffee", "config.yaml")

    let private bookmarkKey char = sprintf "_%c" char

    type Config with
        member this.Load () =
            if not <| File.Exists filePath then
                let dir = Path.GetDirectoryName filePath
                if not <| Directory.Exists dir then
                    Directory.CreateDirectory dir |> ignore
                this.Bookmarks.Clear()
                File.WriteAllText(filePath, this.ToString())
            this.LoadAndWatch filePath |> ignore

        member this.Save () = this.Save filePath

        member this.PathFormat
            with get () = ParseUnionCase<PathFormat> this.PathFormatName |> Option.coalesce Windows
            and set value = this.PathFormatName <- GetUnionCaseName value

        member this.StartupPath
            with get () = ParseUnionCase<StartupPath> this.StartupPathType |> Option.coalesce RestorePrevious
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
            let sorted = this.Bookmarks |> Seq.sortBy sortKey |> Seq.toList
            this.Bookmarks.Clear()
            sorted |> Seq.iter this.Bookmarks.Add

        member this.RemoveBookmark char =
            let key = bookmarkKey char
            this.Bookmarks |> Seq.tryFindIndex (fun b -> b.Key = key)
                           |> Option.iter this.Bookmarks.RemoveAt

        member this.GetBookmark char =
            let key = bookmarkKey char
            this.Bookmarks |> Seq.tryFind (fun b -> b.Key = key) |> Option.map (fun b -> b.Path)

        member this.GetBookmarks () =
            this.Bookmarks |> Seq.map (fun b -> b.Key.[1], b.Path) |> dict
