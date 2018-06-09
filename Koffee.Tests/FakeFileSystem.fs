namespace Koffee

type FakeFileSystemReader() =
    let unset () = (fun _ -> failwith "Unexpected call")

    member val GetNode = unset () with get, set
    member val GetNodes = unset () with get, set
    member val IsEmpty = unset () with get, set

    interface IFileSystemReader with
        member this.GetNode path = this.GetNode path
        member this.GetNodes showHidden path = this.GetNodes showHidden path
        member this.IsEmpty path = this.IsEmpty path
