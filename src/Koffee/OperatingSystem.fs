namespace Koffee

open System.Diagnostics
open Acadian.FSharp

module OsInterop =
    open System
    open System.Runtime.InteropServices

    // Open With dialog: http://www.pinvoke.net/default.aspx/shell32/SHOpenWithDialog.html
    module private OpenWithNative =
        type OpenWithFlags =
            | ALLOW_REGISTRATION = 0x01 // Show "Always" checkbox
            | REGISTER_EXT = 0x02       // Perform registration when user hits OK
            | EXEC = 0x04               // Exec file after registering
            | FORCE_REGISTRATION = 0x08 // Force the checkbox to be registration
            | HIDE_REGISTRATION = 0x20  // Vista+: Hide the "always use this file" checkbox
            | URL_PROTOCOL = 0x40       // Vista+: cszFile is actually a URI scheme; show handlers for that scheme
            | FILE_IS_URI = 0x80        // Win8+: The location pointed to by the pcszFile parameter is given as a URI

        [<Struct>]
        type OpenWithInfo =
            [<MarshalAs(UnmanagedType.LPWStr)>]
            val mutable cszFile: string
            [<MarshalAs(UnmanagedType.LPWStr)>]
            val mutable cszClass: string
            [<MarshalAs(UnmanagedType.I4)>]
            val mutable oaifInFlags: OpenWithFlags

        [<DllImport("shell32.dll", EntryPoint = "SHOpenWithDialog", CharSet = CharSet.Unicode)>]
        extern int SHOpenWithDialog(IntPtr hWndParent, OpenWithInfo& info)

    open OpenWithNative

    let openFileWith fileName =
        let mutable info =
            OpenWithInfo(
                cszFile = fileName,
                cszClass = "",
                oaifInFlags = (OpenWithFlags.ALLOW_REGISTRATION ||| OpenWithFlags.EXEC)
            )
        SHOpenWithDialog(IntPtr.Zero, &info)

type IOperatingSystem =
    abstract member OpenFile: Path -> Result<unit, exn>
    abstract member OpenFileWith: Path -> Result<unit, exn>
    abstract member OpenExplorer: Node -> unit
    abstract member LaunchApp: exePath: string -> workingPath: Path -> args: string -> Result<unit, exn>

type OperatingSystem() =
    let wpath (path: Path) = path.Format Windows

    interface IOperatingSystem with
        member this.OpenFile path =
            tryResult <| fun () ->
                ProcessStartInfo(wpath path, WorkingDirectory = wpath path.Parent)
                |> Process.Start |> ignore

        member this.OpenFileWith path =
            tryResult <| fun () ->
                OsInterop.openFileWith (wpath path) |> ignore

        member this.OpenExplorer node =
            match node.Type with
            | File | Folder when node.Path <> Path.Root ->
                Process.Start("explorer.exe", sprintf "/select,\"%s\"" (wpath node.Path)) |> ignore
            | Drive | Empty ->
                Process.Start("explorer.exe", sprintf "\"%s\"" (wpath node.Path)) |> ignore
            | _ ->
                Process.Start("explorer.exe") |> ignore

        member this.LaunchApp exePath workingPath args =
            tryResult <| fun () ->
                ProcessStartInfo(exePath, args, WorkingDirectory = wpath workingPath)
                |> Process.Start |> ignore
