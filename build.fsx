#r "paket:
nuget Fake.Core.Target
nuget Fake.Core.ReleaseNotes
nuget Fake.IO.FileSystem
nuget Fake.IO.Zip
nuget Fake.DotNet.AssemblyInfoFile
nuget Fake.DotNet.Paket
nuget Fake.DotNet.MSBuild
nuget Fake.DotNet.Testing.NUnit
nuget Fake.Installer.InnoSetup
nuget Fake.API.GitHub //"
#load "./.fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.DotNet
open Fake.DotNet.Testing
open Fake.Installer
open Fake.Api
open System.IO
open System.Security.Cryptography

let buildDir = "build/"
let testDir = "test/"
let distConfigDir = "dist-config/"
let distFilesDir = "dist-files/"
let distDir = "dist/"

let summary = "Fast, keyboard-driven file explorer."
let description =
    "The Keyboard-Oriented File and Folder Explorer for Efficiency, or Koffee, is a no-nonsense alternative to " +
    "Windows Explorer focused on VIM-style keyboard shortcuts, speed, and simplicity."

let releaseNotes = ReleaseNotes.load "RELEASE_NOTES.md"
let version = releaseNotes.AssemblyVersion

let failIfNonZero ret =
    if ret <> 0 then failwith "Shell command failed."

Target.create "clean" (fun _ ->
    Shell.cleanDirs [
        buildDir
        testDir
        distFilesDir
    ]
)

Target.create "version" (fun _ ->
    AssemblyInfoFile.updateAttributes "src/Koffee/AssemblyInfo.fs" [
        AssemblyInfo.Description description
        AssemblyInfo.Version version
        AssemblyInfo.FileVersion version
    ]
)

Target.create "restore" (fun _ ->
    Paket.restore (fun p -> { p with WorkingDir = "src/" })
)

let buildParams (p: MSBuildParams) =
    { p with
        RestorePackagesFlag = true
        Verbosity = Some Quiet
    }

Target.create "build" (fun _ ->
    !! "src/Koffee/*.fsproj"
    |> MSBuild.runRelease buildParams buildDir "Build"
    |> ignore
)

Target.create "buildtest" (fun _ ->
    !! "src/Koffee.Tests/*.fsproj"
    |> MSBuild.runDebug buildParams testDir "Build"
    |> ignore
)

Target.create "test" (fun _ ->
    !! (testDir + "Koffee.Tests.dll")
    |> NUnit3.run (fun p ->
        { p with
            ShadowCopy = false
            ToolPath = "src/packages/NUnit.ConsoleRunner/tools/nunit3-console.exe"
        })
)

Target.create "install" (fun _ ->
    let execElevated cmd (args: string) =
        let args = args.Replace("\"", "\"\"\"")
        Shell.Exec("powershell", sprintf @"start -verb runas %s -argumentlist '%s'" cmd args)
    let bin = Path.getFullName buildDir
    let progFiles = Environment.environVarOrDefault "ProgramFiles(x86)" (Environment.environVar "ProgramFiles")
    let installDir = progFiles + @"\Koffee"
    execElevated "robocopy" (sprintf "\"%s\" \"%s\" *.exe* *.dll" bin installDir)
    |> failIfNonZero
    Trace.tracefn "Installed in: %s" installDir

    // create shortcut
    let execPowershell (command: string) =
        let command = command.Replace("\"", "\"\"\"").Replace("\n", "; ")
        Shell.Exec("powershell", sprintf "-command \"%s\"" command)
    execPowershell (sprintf @"
        $lnkPath = ""$([Environment]::GetFolderPath('StartMenu'))\Programs\Koffee.lnk""
        $s = (New-Object -ComObject WScript.Shell).CreateShortcut($lnkPath)
        $s.TargetPath = ""%s\Koffee.exe""
        $s.Save()" installDir)
    |> failIfNonZero
)

let zipFile = sprintf "%sKoffee-%s.zip" distDir version
let installerBaseFileName = sprintf "Koffee-Setup-%s" version
let installerFile = distDir + installerBaseFileName + ".exe"

Target.create "package" (fun _ ->
    Directory.create distDir

    // setup dir with files to distribute
    !! (buildDir + "*.exe*")
    ++ (buildDir + "*.dll")
    ++ ("*.md")
    ++ ("*.txt")
    |> Shell.copy (distFilesDir + "Koffee")

    // create zip
    !! (distFilesDir + "Koffee/*")
    |> Zip.zip distFilesDir zipFile

    let computeHash file =
        use stream = File.OpenRead(file)
        use sha = new SHA256Managed()
        let checksum = sha.ComputeHash(stream)
        System.BitConverter.ToString(checksum).Replace("-", "").ToLower()
    let zipHash = computeHash zipFile

    let substitutions = [
        ("!version!", version)
        ("!summary!", summary)
        ("!description!", description)
        ("!zipHash!", zipHash)
    ]
    let substitute destDir source =
        let destFile = destDir + Path.GetFileName source
        (File.ReadAllText source, substitutions)
        ||> Seq.fold (fun text (key, sub) -> text.Replace(key, sub))
        |> (fun text -> File.WriteAllText(destFile, text))
        destFile

    // create installer
    let installerScript = substitute distFilesDir (distConfigDir + "installer.iss")
    InnoSetup.build (fun p ->
        { p with
            ScriptFile = installerScript
            QuietMode = InnoSetup.QuietAndProgress
            OutputBaseFilename = installerBaseFileName
        })

    // create Chocolatey package
    let chocoDir = distFilesDir + "choco/"
    Shell.copyRecursive (distConfigDir + "choco") chocoDir true |> ignore
    Shell.copyRecursive (distFilesDir + "Koffee") (chocoDir + "tools/") true |> ignore
    let chocoSpec = substitute chocoDir (distConfigDir + "choco/koffee.nuspec")
    Shell.Exec("choco", sprintf "pack %s --out %s" chocoSpec distDir)
    |> failIfNonZero

    // create Scoop manifest
    substitute distDir (distConfigDir + "koffee.json") |> ignore

    Trace.tracefn "Release files have been created in: %s" distDir
)

Target.create "publish" (fun _ ->
    let token =
       match Environment.environVarOrNone "koffee_deploy_token" with
       | Some s -> s
       | None -> failwith "Set the koffee_deploy_token environment variable to a github personal access token with repo access."
    GitHub.createClientWithToken token
    |> GitHub.draftNewRelease "mattstermiller" "koffee" ("v" + version) false releaseNotes.Notes
    |> GitHub.uploadFiles [zipFile; installerFile]
    |> GitHub.publishDraft
    |> Async.RunSynchronously

    let chocoPackage = sprintf "%skoffee.%s.nupkg" distDir version
    Shell.Exec("choco", sprintf "push \"%s\" --source https://chocolatey.org/" chocoPackage)
    |> failIfNonZero
)

open Fake.Core.TargetOperators

"clean"
    ==> "version"
    ==> "restore"
    ==> "build"
    ==> "package"
    <=> "install"

"restore"
    ==> "buildtest"
    ==> "test"
    ==> "package"
    <=> "install"

"package"
    ==> "publish"

Target.runOrDefault "build"
