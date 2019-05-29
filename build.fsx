#r "paket:
nuget Fake.Core.Target
nuget Fake.Core.ReleaseNotes
nuget Fake.IO.FileSystem
nuget Fake.IO.Zip
nuget Fake.DotNet.AssemblyInfoFile
nuget Fake.DotNet.Paket
nuget Fake.DotNet.MSBuild
nuget Fake.DotNet.Testing.NUnit
nuget Fake.Installer.InnoSetup //"
#load "./.fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.DotNet
open Fake.DotNet.Testing
open Fake.Installer
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
    AssemblyInfoFile.updateAttributes "Koffee/AssemblyInfo.fs" [
        AssemblyInfo.Description description
        AssemblyInfo.Version version
        AssemblyInfo.FileVersion version
    ]
)

Target.create "restore" (fun _ ->
    Paket.restore id
)

let buildParams (p: MSBuildParams) =
    { p with
        RestorePackagesFlag = true
        Verbosity = Some Quiet
    }

Target.create "build" (fun _ ->
    !! "Koffee/*.fsproj"
    |> MSBuild.runRelease buildParams buildDir "Build"
    |> ignore
)

Target.create "buildtest" (fun _ ->
    !! "Koffee.Tests/*.fsproj"
    |> MSBuild.runDebug buildParams testDir "Build"
    |> ignore
)

Target.create "test" (fun _ ->
    !! (testDir + "Koffee.Tests.dll")
    |> NUnit3.run (fun p ->
        { p with
            ShadowCopy = false
            ToolPath = "packages/NUnit.ConsoleRunner/tools/nunit3-console.exe"
        })
)

Target.create "package" (fun _ ->
    Directory.create distDir

    // setup dir with files to distribute
    !! (buildDir + "*.exe*")
    ++ (buildDir + "*.dll")
    ++ ("*.md")
    ++ ("*.txt")
    |> Shell.copy (distFilesDir + "Koffee")

    // create zip
    let zipFile = sprintf "Koffee-%s.zip" version
    !! (distFilesDir + "Koffee/*")
    |> Zip.zip distFilesDir (distDir + zipFile)

    let computeHash file =
        use stream = File.OpenRead(file)
        use sha = new SHA256Managed()
        let checksum = sha.ComputeHash(stream)
        System.BitConverter.ToString(checksum).Replace("-", "").ToLower()
    let zipHash = computeHash (distDir + zipFile)

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
    let installer = substitute distFilesDir (distConfigDir + "installer.iss")
    InnoSetup.build (fun p ->
        { p with
            ScriptFile = installer
            QuietMode = InnoSetup.QuietAndProgress
        })

    // create Chocolatey package
    let chocoDir = distFilesDir + "choco/"
    Shell.copyRecursive (distConfigDir + "choco") chocoDir true |> ignore
    Shell.copyRecursive (distFilesDir + "Koffee") (chocoDir + "tools/") true |> ignore
    let chocoSpec = substitute chocoDir (distConfigDir + "choco/koffee.nuspec")
    Shell.Exec("choco", sprintf "pack %s --out %s" chocoSpec distDir)
    |> failIfNonZero

    // create Scoop manifest
    // TODO: move to publish step
    substitute "./" (distConfigDir + "koffee.json") |> ignore

    Trace.tracefn "Release files have been created in: %s" distDir
)

open Fake.Core.TargetOperators

"clean"
    ==> "version"
    ==> "restore"
    ==> "build"
    ==> "buildtest"
    ==> "test"
    ==> "package"

Target.runOrDefault "test"
