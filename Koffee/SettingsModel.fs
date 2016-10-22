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
    | NonEvent
