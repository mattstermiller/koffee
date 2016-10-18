namespace Koffee

open System
open FSharp.Desktop.UI

type Path =
    | Path of string
    member this.Value =
        let (Path p) = this
        p

type NodeType =
    | File
    | Folder
    | Drive
    | Error

type Node = {
    Path: Path
    Name: string
    Type: NodeType
    Modified: DateTime option
    Size: int64 option
}

type Node with
    member this.SizeFormatted =
        if this.Size.IsSome then
            let scale level = pown 1024L level
            let scaleCutoff level = 10L * (scale level)
            let scaledStr size level =
                let scaled = size / (scale level)
                let levelName = "KB,MB,GB".Split(',').[level-1]
                scaled.ToString("N0") + " " + levelName
            match this.Size.Value with
                | size when size > scaleCutoff 3 -> scaledStr size 3
                | size when size > scaleCutoff 2 -> scaledStr size 2
                | size when size > scaleCutoff 1 -> scaledStr size 1
                | size -> size.ToString("N0")
        else ""

[<AbstractClass>]
type MainModel() =
    inherit Model()

    abstract Path: Path with get, set
    abstract Status: string with get, set
    abstract Nodes: Node list with get, set
    abstract Cursor: int with get, set
    abstract PageSize: int with get, set
    abstract LastFind: char option with get, set
    abstract LastSearch: string option with get, set

    member this.SelectedNode = this.Nodes.[this.Cursor]
    member this.HalfPageScroll = this.PageSize/2 - 1

type CommandInput =
    | FindInput
    | SearchInput

type MainEvents =
    | NavUp
    | NavUpHalfPage
    | NavDown
    | NavDownHalfPage
    | NavToFirst
    | NavToLast
    | OpenPath of string
    | OpenSelected
    | OpenParent
    | OpenExplorer
    | StartInput of CommandInput
    | Find of char
    | FindNext
    | Search of string
    | SearchNext
    | TogglePathFormat
