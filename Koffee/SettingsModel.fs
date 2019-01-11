namespace Koffee

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
    | StartPathChanged of StartPath
    | DefaultPathChanged of string
    | TextEditorChanged of string
    | CommandlinePathChanged of string
    | PathFormatChanged of PathFormat
    | ShowFullPathInTitleChanged of bool
    | ShowHiddenChanged of bool
    | SearchCaseSensitiveChanged of bool
    | RefreshWindowOnActivate of bool
