namespace Koffee

open System
open FSharp.Desktop.UI

type KeyBind = {
    EventName: string
    BoundKeys: string
}

[<AbstractClass>]
type SettingsModel() =
    inherit Model()
    abstract KeyBindings: KeyBind list with get, set

type SettingsEvents =
    | StartupPathChanged of StartupPath
    | DefaultPathChanged of string
    | TextEditorChanged of string
    | CommandlinePathChanged of string
    | PathFormatChanged of PathFormat
    | ShowFullPathInTitleChanged of bool
    | ShowHiddenChanged of bool
    | RefreshWindowOnActivate of bool
