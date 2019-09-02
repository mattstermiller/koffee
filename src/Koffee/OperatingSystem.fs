namespace Koffee

open System.Diagnostics
open System.Windows.Forms
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


type IOperatingSystem =
    abstract member OpenFile: Path -> Result<unit, exn>
    abstract member OpenFileWith: Path -> Result<unit, exn>
    abstract member OpenProperties: Path -> Result<unit, exn>
    abstract member OpenExplorer: Item -> unit
    abstract member LaunchApp: exePath: string -> workingPath: Path -> args: string -> Result<unit, exn>
    abstract member CopyToClipboard: Path -> Result<unit, exn>

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

        member this.OpenExplorer item =
            match item.Type with
            | File | Folder when item.Path <> Path.Root ->
                Process.Start("explorer.exe", sprintf "/select,\"%s\"" (wpath item.Path)) |> ignore
            | Drive | Empty ->
                Process.Start("explorer.exe", sprintf "\"%s\"" (wpath item.Path)) |> ignore
            | _ ->
                Process.Start("explorer.exe") |> ignore

        member this.LaunchApp exePath workingPath args =
            tryResult <| fun () ->
                ProcessStartInfo(exePath, args, WorkingDirectory = wpath workingPath)
                |> Process.Start |> ignore

        member this.CopyToClipboard path =
            tryResult <| fun () ->
                let path = wpath path
                let data = DataObject(DataFormats.FileDrop, [|path|])
                data.SetText(path)
                Clipboard.SetDataObject(data, true)

module LinkFile =
    open System
    open System.Text
    open System.Runtime.InteropServices
    open System.Runtime.InteropServices.ComTypes

    type IShellLinkGetPathFlags =
        | ShortPath = 1
        | UncPriority = 2
        | RawPath = 4

    [<ComImport>]
    [<InterfaceType(ComInterfaceType.InterfaceIsIUnknown)>]
    [<Guid("000214F9-0000-0000-C000-000000000046")>]
    type private IShellLink =
        abstract member GetPath: [<Out; MarshalAs(UnmanagedType.LPWStr)>] pszFile: StringBuilder -> cchMaxPath: int ->
                                 [<Out>] pfd: IntPtr -> fFlags: IShellLinkGetPathFlags -> unit
        abstract member GetIDList: [<Out>] ppidl: IntPtr -> unit
        abstract member SetIDList: pidl: IntPtr -> unit
        abstract member GetDescription: [<Out; MarshalAs(UnmanagedType.LPWStr)>] pszName: StringBuilder -> cchMaxName: int -> unit
        abstract member SetDescription: [<MarshalAs(UnmanagedType.LPWStr)>] pszName: string -> unit
        abstract member GetWorkingDirectory: [<Out; MarshalAs(UnmanagedType.LPWStr)>] pszDir: StringBuilder -> cchMaxPath: int -> unit
        abstract member SetWorkingDirectory: [<MarshalAs(UnmanagedType.LPWStr)>] pszDir: string -> unit
        abstract member GetArguments: [<Out; MarshalAs(UnmanagedType.LPWStr)>] pszArgs: StringBuilder -> cchMaxPath: int -> unit
        abstract member SetArguments: [<MarshalAs(UnmanagedType.LPWStr)>] pszArgs: string -> unit
        abstract member GetHotkey: [<Out>] pwHotkey: int16 -> unit
        abstract member SetHotkey: wHotkey: int16 -> unit
        abstract member GetShowCmd: [<Out>] piShowCmd: int -> unit
        abstract member SetShowCmd: iShowCmd: int -> unit
        abstract member GetIconLocation: [<Out; MarshalAs(UnmanagedType.LPWStr)>] pszIconPath: StringBuilder -> cchIconPath: int -> [<Out>] piIcon: int -> unit
        abstract member SetIconLocation: [<MarshalAs(UnmanagedType.LPWStr)>] pszIconPath: string -> iIcon: int -> unit
        abstract member SetRelativePath: [<MarshalAs(UnmanagedType.LPWStr)>] pszPathRel: string -> dwReserved: int -> unit
        abstract member Resolve: hwnd: IntPtr -> fFlags: int -> unit
        abstract member SetPath: [<MarshalAs(UnmanagedType.LPWStr)>] pszFile: string -> unit

    let private ShellLink () =
        let typ = Type.GetTypeFromCLSID (Guid "00021401-0000-0000-C000-000000000046")
        Activator.CreateInstance typ :?> IShellLink

    let getLinkTarget path =
        let link = ShellLink ()
        (link :?> IPersistFile).Load(path, 0)
        let target = StringBuilder(260)
        link.GetPath target target.Capacity IntPtr.Zero IShellLinkGetPathFlags.RawPath
        target.ToString()

    let saveLink target path =
        let link = ShellLink ()
        link.SetPath target
        (link :?> IPersistFile).Save(path, false)
