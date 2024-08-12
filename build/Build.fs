open Fake.Core
open Fake.Core.TargetOperators
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.DotNet
open Fake.Api
open System.IO
open System.Security.Cryptography

let buildOutputDir = "src/Koffee/bin/Release/net451/"
let distConfigDir = "dist-config/"
let distStagingDir = "dist-staging/"
let distDir = "dist/"

let summary = "Fast, keyboard-driven file explorer."
let description =
    "The Keyboard-Oriented File and Folder Explorer for Efficiency, or Koffee, is a no-nonsense alternative to " +
    "Windows Explorer focused on VIM-style keyboard shortcuts, speed, and simplicity."

let releaseNotes = ReleaseNotes.load "RELEASE_NOTES.md"
let version = releaseNotes.AssemblyVersion

let failIfNonZero ret =
    if ret <> 0 then failwith "Shell command failed."

let initTargets () =
    Target.create "clean" (fun _ ->
        let clean config =
            Shell.Exec("dotnet", sprintf "clean --verbosity minimal --configuration %s" config)
            |> failIfNonZero
        clean "debug"
        clean "release"

        Shell.cleanDirs [distStagingDir]
    )

    Target.create "version" (fun _ ->
        AssemblyInfoFile.updateAttributes "src/Koffee/AssemblyInfo.fs" [
            AssemblyInfo.Description description
            AssemblyInfo.Version version
            AssemblyInfo.FileVersion version
        ]
    )

    Target.create "buildApp" (fun _ ->
        DotNet.build (fun opt -> { opt with Configuration = DotNet.Release }) "src/Koffee"
    )

    Target.create "buildAppDebug" (fun _ ->
        DotNet.build (fun opt -> { opt with Configuration = DotNet.Debug }) "src/Koffee"
    )

    Target.create "buildAllDebug" (fun _ ->
        DotNet.build (fun opt -> { opt with Configuration = DotNet.Debug }) ""
    )

    Target.create "test" (fun _ ->
        DotNet.test (fun opt -> { opt with Configuration = DotNet.Release }) ""
    )

    Target.create "install" (fun _ ->
        let bin = Path.getFullName buildOutputDir
        let progFiles = Environment.environVarOrDefault "ProgramFiles(x86)" (Environment.environVar "ProgramFiles")
        let installDir = progFiles + @"\Koffee"

        // copy files to program files
        let execElevated cmd (args: string) =
            Shell.Exec("powershell", sprintf @"start -verb runas %s -argumentlist '%s'" cmd args)
        let innerQuote (s: string) =
            s.Trim('\\') |> sprintf "\\\"%s\\\""
        printfn "%s %s *.exe* *.dll" (innerQuote bin) (innerQuote installDir)
        execElevated "robocopy" (sprintf "%s %s *.exe* *.dll" (innerQuote bin) (innerQuote installDir))
        |> failIfNonZero
        Trace.tracefn "Installed in: %s" installDir

        // create shortcut
        let quote (s: string) =
            s.Replace("\"", "\\\"") |> sprintf "\"%s\""
        let execPowershell (command: string) =
            let command = command.Replace("\n", "; ")
            Shell.Exec("powershell", sprintf "-command %s" (quote command))
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
        !! (buildOutputDir + "*.exe*")
        ++ (buildOutputDir + "*.dll")
        ++ ("*.md")
        ++ ("*.txt")
        |> Shell.copy (distStagingDir + "Koffee")

        // create zip
        !! (distStagingDir + "Koffee/*")
        |> Zip.zip distStagingDir zipFile

        let computeHash file =
            use stream = File.OpenRead(file)
            use sha = SHA256.Create()
            let checksum = sha.ComputeHash(stream)
            System.BitConverter.ToString(checksum).Replace("-", "").ToLower()
        let zipHash = computeHash zipFile

        let substitutions = [
            ("!version!", version)
            ("!summary!", summary)
            ("!description!", description)
            ("!zipHash!", zipHash)
        ]
        let substitute destDir (source: string) =
            let destFile = destDir + Path.GetFileName source
            (File.ReadAllText source, substitutions)
            ||> Seq.fold (fun text (key, sub) -> text.Replace(key, sub))
            |> (fun text -> File.WriteAllText(destFile, text))
            destFile

        // create installer
        let installerScript = substitute distStagingDir (distConfigDir + "installer.iss")
        Shell.Exec("dotnet", sprintf "iscc /F\"%s\" /Q \"%s\"" installerBaseFileName installerScript)
        |> failIfNonZero

        // create Chocolatey package
        let chocoDir = distStagingDir + "choco/"
        Shell.copyRecursive (distConfigDir + "choco") chocoDir true |> ignore
        Shell.copyRecursive (distStagingDir + "Koffee") (chocoDir + "tools/") true |> ignore
        let chocoSpec = substitute chocoDir (distConfigDir + "choco/koffee.nuspec")
        Shell.Exec("choco", sprintf "pack %s --out %s" chocoSpec distDir)
        |> failIfNonZero

        // create Scoop manifest
        substitute distDir (distConfigDir + "koffee.json") |> ignore

        Trace.tracefn "Release files have been created in: %s" distDir
    )

    Target.create "publish" (fun _ ->
        let githubToken =
            Environment.environVarOrNone "koffee_deploy_token"
            |> Option.defaultWith (fun () ->
                failwith "Set the 'koffee_deploy_token' environment variable to a github personal access token with 'repo' access."
            )
        let chocoApiKey =
            Environment.environVarOrNone "choco_api_key"
            |> Option.defaultWith (fun () ->
                failwith "Set the 'choco_api_key' environment variable to your chocolatey API key"
            )

        GitHub.createClientWithToken githubToken
        |> GitHub.draftNewRelease "mattstermiller" "koffee" ("v" + version) false releaseNotes.Notes
        |> GitHub.uploadFiles [zipFile; installerFile]
        |> GitHub.publishDraft
        |> Async.RunSynchronously

        let chocoPackage = sprintf "%skoffee.%s.nupkg" distDir version
        Shell.Exec("choco", sprintf "push \"%s\" --source https://chocolatey.org/ --api-key=\"%s\"" chocoPackage chocoApiKey)
        |> failIfNonZero
    )

    let (==>!) x y =
        x ==> y |> ignore
    let (?=>!) x y =
        x ?=> y |> ignore

    // all build targets depend on updating version
    "version" ==>! "buildApp"
    "version" ==>! "buildAppDebug"
    "version" ==>! "buildAllDebug"

    "package" <== [
        "clean"
        "buildApp"
        "test"
    ]

    "package" ==>! "publish"

    "install" <== [
        "buildApp"
        "test"
    ]

    // clean must be run before build or test
    "clean" ?=>! "buildApp"
    "clean" ?=>! "test"

    "buildApp" ?=>! "test"

[<EntryPoint>]
let main argv =
    argv
    |> Array.toList
    |> fun args ->
        match args with
        | a :: _ when not (a.StartsWith "-") -> "-t" :: args
        | _ -> args
    |> Context.FakeExecutionContext.Create false "build.fsx"
    |> Context.RuntimeContext.Fake
    |> Context.setExecutionContext

    initTargets ()
    Target.runOrDefault "buildAllDebug"
    0
