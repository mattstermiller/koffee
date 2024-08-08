namespace Koffee

open System
open System.Collections.Specialized
open System.Diagnostics
open System.Windows.Forms
open Acadian.FSharp

module OsInterop =
    open System.Runtime.InteropServices
    open System.Runtime.InteropServices.ComTypes

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
        type HRESULT =
            | S_OK = 0

        [<DllImport("Shell32.dll", CharSet = CharSet.Unicode, SetLastError = true)>]
        extern HRESULT SHILCreateFromPath([<MarshalAs(UnmanagedType.LPWStr)>] string pszPath, IntPtr& ppIdl, uint& rgflnOut)

        [<DllImport("Shell32.dll", CharSet = CharSet.Unicode, SetLastError = true)>]
        extern IntPtr ILFindLastID(IntPtr pidl)

        [<DllImport("Shell32.dll", CharSet = CharSet.Unicode, SetLastError = true)>]
        extern IntPtr ILClone(IntPtr pidl)

        [<DllImport("Shell32.dll", CharSet = CharSet.Unicode, SetLastError = true)>]
        extern Boolean ILRemoveLastID(IntPtr pidl)

        [<DllImport("Shell32.dll", CharSet = CharSet.Unicode, SetLastError = true)>]
        extern void ILFree(IntPtr pidl)

        [<DllImport("Shell32.dll", CharSet = CharSet.Unicode, SetLastError = true)>]
        extern HRESULT CIDLData_CreateFromIDArray(IntPtr pidlFolder, uint cidl, IntPtr[] apidl, IDataObject& ppdtobj)

        [<DllImport("Shell32.dll", CharSet = CharSet.Unicode, SetLastError = true)>]
        extern HRESULT SHMultiFileProperties(IDataObject pdtobj, uint dwFlags)

    module private SelectInExplorer =
        [<DllImport("shell32.dll", SetLastError = true)>]
        extern int SHOpenFolderAndSelectItems(IntPtr pidlFolder, uint cidl,
            [<In; MarshalAs(UnmanagedType.LPArray)>] IntPtr[] apidl, uint dwFlags)

        [<DllImport("shell32.dll", SetLastError = true)>]
        extern void SHParseDisplayName([<MarshalAs(UnmanagedType.LPWStr)>] string name, IntPtr bindingContext,
            [<Out>] IntPtr& pidl, uint sfgaoIn, [<Out>] uint& psfgaoOut);


    open OpenWithNative
    open OpenPropertiesNative
    open SelectInExplorer

    let openFileWith fileName =
        let mutable info =
            OpenWithInfo(
                cszFile = fileName,
                cszClass = "",
                oaifInFlags = (OpenWithFlags.ALLOW_REGISTRATION ||| OpenWithFlags.EXEC)
            )
        SHOpenWithDialog(IntPtr.Zero, &info)

    let openProperties paths =
        let mutable pidlParent = IntPtr.Zero
        let pidlItems = ResizeArray()

        let loadItemId path =
            let mutable pidlFull = IntPtr.Zero
            let mutable rgflnOut = 0u
            let hres = SHILCreateFromPath(path, &pidlFull, &rgflnOut)
            if hres = HRESULT.S_OK then
                let pidlItem = ILFindLastID(pidlFull)
                pidlItems.Add(ILClone(pidlItem))
                ILRemoveLastID(pidlFull) |> ignore
                if pidlParent = IntPtr.Zero then
                    pidlParent <- ILClone(pidlFull)
                ILFree(pidlFull)
            // ignore item failure

        paths |> Seq.iter loadItemId
        if pidlItems.Count = 0 then
            failwith "Failed to load any items for opening property window"

        try
            let pidlItemArray = pidlItems |> Seq.toArray
            let mutable pDataObj = null
            let mutable hres = CIDLData_CreateFromIDArray(pidlParent, uint32 pidlItemArray.Length, pidlItemArray, &pDataObj)
            if hres = HRESULT.S_OK then
                hres <- SHMultiFileProperties(pDataObj, 0u)

            if hres <> HRESULT.S_OK then
                failwith "Failed to open multi-file properties"
        finally
            seq {
                pidlParent
                yield! pidlItems
            }
            |> Seq.iter ILFree

    let openExplorerAndSelect location selectItemPaths =
        let getDisplayName path =
            let mutable nativePath = IntPtr.Zero
            let mutable psfgaoOut = 0u
            SHParseDisplayName(path, IntPtr.Zero, &nativePath, 0u, &psfgaoOut)
            Some nativePath |> Option.filter ((<>) IntPtr.Zero)
        let nativeLocation =
            getDisplayName location
            |> Option.defaultWith (fun () -> failwithf "Cannot read folder %s" location)
        let nativeSelectPaths =
            selectItemPaths
            |> Seq.choose getDisplayName
            |> Seq.ifEmpty (seq {IntPtr.Zero})
            |> Seq.toArray
        try
            SHOpenFolderAndSelectItems(nativeLocation, uint nativeSelectPaths.Length, nativeSelectPaths, 0u)
        finally
            [
                nativeLocation
                yield! nativeSelectPaths
            ]
            |> List.iter Marshal.FreeCoTaskMem


type IOperatingSystem =
    abstract member OpenFile: Path -> Result<unit, exn>
    abstract member OpenFileWith: Path -> Result<unit, exn>
    abstract member OpenProperties: Path seq -> Result<unit, exn>
    abstract member OpenExplorer: location: Path -> selectItemPaths: Path seq -> Result<unit, exn>
    abstract member LaunchApp: exePath: string -> workingPath: Path -> args: string -> Result<unit, exn>
    abstract member GetClipboardFileDrop: unit -> Result<PutType * Path list, exn>
    abstract member SetClipboardFileDrop: Path seq -> Result<unit, exn>
    abstract member SetClipboardText: string -> Result<unit, exn>
    abstract member GetEnvironmentVariable: string -> string option

type OperatingSystem() =
    let wpath (path: Path) = path.Format Windows

    let dropEffectDataName = "Preferred DropEffect"

    let setClipboardData data =
        Clipboard.SetDataObject(data, true, 3, 250)

    interface IOperatingSystem with
        member _.OpenFile path =
            tryResult <| fun () ->
                ProcessStartInfo(wpath path, WorkingDirectory = wpath path.Parent)
                |> Process.Start |> ignore

        member _.OpenFileWith path =
            tryResult <| fun () ->
                OsInterop.openFileWith (wpath path) |> ignore

        member _.OpenProperties paths =
            tryResult <| fun () ->
                OsInterop.openProperties (paths |> Seq.map wpath) |> ignore

        member _.OpenExplorer location selectItemPaths =
            tryResult <| fun () ->
                OsInterop.openExplorerAndSelect (wpath location) (selectItemPaths |> Seq.map wpath) |> ignore

        member _.LaunchApp exePath workingPath args =
            tryResult <| fun () ->
                ProcessStartInfo(exePath, args, WorkingDirectory = wpath workingPath)
                |> Process.Start |> ignore

        member _.GetClipboardFileDrop () =
            tryResult <| fun () ->
                let putType =
                    Clipboard.GetData(dropEffectDataName)
                    |> Option.ofObj
                    |> Option.bind (fun data ->
                        let stream = data :?> IO.MemoryStream
                        let effectBytes = Array.zeroCreate 4
                        stream.Read(effectBytes, 0, effectBytes.Length) |> ignore
                        BitConverter.ToInt32(effectBytes, 0)
                        |> fun i -> Enum.ToObject(typeof<Windows.DragDropEffects>, i) :?> Windows.DragDropEffects
                        |> DragDropEffects.toPutTypes
                        |> List.tryHead
                    )
                    |? Copy
                let paths =
                    Clipboard.GetFileDropList()
                    |> Option.ofObj
                    |? StringCollection()
                    |> Seq.cast<string>
                    |> Seq.choose Path.Parse
                    |> Seq.toList
                (putType, paths)

        member _.SetClipboardFileDrop paths =
            tryResult <| fun () ->
                let winPaths = paths |> Seq.map wpath |> Seq.toArray
                DataObject(DataFormats.FileDrop, winPaths)
                |> setClipboardData

        member _.SetClipboardText text =
            tryResult <| fun () ->
                DataObject(DataFormats.Text, text)
                |> setClipboardData

        member _.GetEnvironmentVariable key =
            Environment.GetEnvironmentVariable(key, EnvironmentVariableTarget.Process)
            |> Option.ofObj

module LinkFile =
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
