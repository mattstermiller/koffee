namespace Koffee

type FakeFileSystemReader() =
    let unset () = (fun _ -> failwith "Unexpected call")

    member val GetItem = unset () with get, set
    member val GetItems = unset () with get, set
    member val GetFolders = unset () with get, set
    member val IsEmpty = unset () with get, set
    member val GetShortcutTarget = unset () with get, set

    interface IFileSystemReader with
        member this.GetItem path = this.GetItem path
        member this.GetItems path = this.GetItems path
        member this.GetFolders path = this.GetFolders path
        member this.IsEmpty path = this.IsEmpty path
        member this.GetShortcutTarget path = this.GetShortcutTarget path

type FakeFileSystemWriter() =
    let unset () = (fun _ -> failwith "Unexpected call")

    member val Create = unset () with get, set
    member val CreateShortcut = unset () with get, set
    member val Move = unset () with get, set
    member val Copy = unset () with get, set
    member val Recycle = unset () with get, set
    member val Delete = unset () with get, set

    interface IFileSystemWriter with
        member this.Create itemType path = this.Create itemType path
        member this.CreateShortcut target path = this.CreateShortcut target path
        member this.Move fromPath toPath = this.Move fromPath toPath
        member this.Copy fromPath toPath = this.Copy fromPath toPath
        member this.Recycle path = this.Recycle path
        member this.Delete path = this.Delete path
