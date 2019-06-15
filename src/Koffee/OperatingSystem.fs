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


    module private OpenPropertiesNative =
        let SW_SHOW = 5
        let SEE_MASK_INVOKEIDLIST = 12u

        [<Struct; StructLayout(LayoutKind.Sequential, CharSet = CharSet.Auto)>]
        type ShellExecuteInfo =
            val mutable cbSize: int
            val mutable fMask: uint32
            val mutable hwnd: IntPtr
            [<MarshalAs(UnmanagedType.LPTStr)>]
            val mutable lpVerb: string
            [<MarshalAs(UnmanagedType.LPTStr)>]
            val mutable lpFile: string
            [<MarshalAs(UnmanagedType.LPTStr)>]
            val mutable lpParameters: string
            [<MarshalAs(UnmanagedType.LPTStr)>]
            val mutable lpDirectory: string
            val mutable nShow: int
            val mutable hInstApp: IntPtr
            val mutable lpIDList: IntPtr
            [<MarshalAs(UnmanagedType.LPTStr)>]
            val mutable lpClass: string
            val mutable hkeyClass: IntPtr
            val mutable dwHotKey: uint32
            val mutable hIcon: IntPtr
            val mutable hProcess: IntPtr

        [<DllImport("shell32.dll", CharSet = CharSet.Auto)>]
        extern bool ShellExecuteEx(ShellExecuteInfo& lpExecInfo);

    open OpenWithNative
    open OpenPropertiesNative

    let openFileWith fileName =
        let mutable info =
            OpenWithInfo(
                cszFile = fileName,
                cszClass = "",
                oaifInFlags = (OpenWithFlags.ALLOW_REGISTRATION ||| OpenWithFlags.EXEC)
            )
        SHOpenWithDialog(IntPtr.Zero, &info)

    let openProperties fileName =
        let mutable info =
            ShellExecuteInfo(
                lpVerb = "properties",
                lpFile = fileName,
                nShow = SW_SHOW,
                fMask = SEE_MASK_INVOKEIDLIST
            )
        info.cbSize <- Marshal.SizeOf(info)
        ShellExecuteEx(&info);

    open IWshRuntimeLibrary

    let getShortcutPath lnkFileName =
        let link = WshShellClass().CreateShortcut(lnkFileName) :?> IWshShortcut
        link.TargetPath

type IOperatingSystem =
    abstract member OpenFile: Path -> Result<unit, exn>
    abstract member OpenFileWith: Path -> Result<unit, exn>
    abstract member OpenProperties: Path -> Result<unit, exn>
    abstract member OpenExplorer: Node -> unit
    abstract member LaunchApp: exePath: string -> workingPath: Path -> args: string -> Result<unit, exn>
    abstract member GetShortcutPath: Path -> Result<string, exn>

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

        member this.OpenProperties path =
            tryResult <| fun () ->
                OsInterop.openProperties (wpath path) |> ignore

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

        member this.GetShortcutPath path =
            tryResult <| fun () ->
                OsInterop.getShortcutPath (wpath path)
