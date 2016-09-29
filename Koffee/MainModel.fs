namespace Koffee

open System
open FSharp.Desktop.UI

type Path = Path of string
type NodeType = File | Folder

type Node = {
    Path: Path
    Name: string
    Type: NodeType
    Modified: DateTime option
    Size: int64 option
}

[<AbstractClass>]
type MainModel() =
    inherit Model()

    abstract Path: Path with get, set
    abstract Nodes: Node list with get, set
    abstract Cursor: int with get, set

    member this.SelectedNode = this.Nodes.[this.Cursor]

type MainEvents =
    | NavUp
    | NavDown
    | PathChanged
    | OpenSelected
    | OpenParent
