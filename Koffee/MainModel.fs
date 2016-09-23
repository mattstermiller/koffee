namespace Koffee

open FSharp.Desktop.UI

type Path = Path of string

type Node = {
    Name: string
    Type: string
    Path: Path
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
