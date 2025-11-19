module Koffee.ItemActionCommands.Tools

open System
open System.Text.RegularExpressions
open Acadian.FSharp
open Koffee

let private variableRegex =
    let var =
        ToolVariable.names
        |> String.concat "|"
        |> sprintf "(%s)(:[^?}]*)?"
    Regex(sprintf @"\{%s(?:\?%s)*\}" var var, RegexOptions.Compiled)

type VariableMatch = {
    Variables: ToolVariable list
    StartIndex: int
    EndIndex: int
    BracketIndexes: (int * int) option
}

let private parseVariable (source: string) (mtch: Match) =
    let createVar name varOptionStr =
        let varOption = varOptionStr |> Option.ofCond String.isNotEmpty |> Option.map (String.substringFrom 1)
        if name = "env"
        then varOption |> Option.map Env
        else Reflection.makeUnion<ToolVariable> (name |> String.replace "_" "") [|varOption|]
    createVar mtch.Groups.[1].Value mtch.Groups.[2].Value
    |> Option.map (fun var ->
        let fallbackOptionCaptures =
            mtch.Groups.[4].Captures
            |> Seq.cast<Capture>
            |> Seq.map (fun c -> c.Index, c.Value)
            |> Map
        let fallbackVars =
            mtch.Groups.[3].Captures
            |> Seq.cast<Capture>
            |> Seq.choose (fun c ->
                let varOption = fallbackOptionCaptures.TryFind (c.Index + c.Length) |? String.Empty
                createVar c.Value varOption
            )
            |> Seq.toList
        let startIndex = mtch.Index
        let endIndex = mtch.Index + mtch.Length - 1
        let startBracket = source.LastIndexOf('[', startIndex) |> Option.ofCond ((<>) -1)
        let endBracket = source.IndexOf(']', endIndex) |> Option.ofCond ((<>) -1)
        {
            Variables = var :: fallbackVars
            StartIndex = startIndex
            EndIndex = endIndex
            BracketIndexes = (startBracket, endBracket) ||> Option.map2 Tuple.pack2
        }
    )

let rec private gitRoot (fs: IFileSystemReader) (path: Path) =
    path.Join ".git"
    |> fs.GetItem
    |> Result.bind (function
        | Some _ -> Ok (Some path)
        | None when path.Parent <> Path.Root -> gitRoot fs path.Parent
        | None -> Ok None
    )

type VariableApplication = {
    Value: string
    PathsUsed: Path list
}

let substituteVariables (fs: IFileSystemReader) (getEnvVar: string -> string option) toolName (model: MainModel) str =
    let isFile (item: Item) = item.Type = File
    let isDirectory (item: Item) = item.Type.IsDirectory
    let quote =
        String.replace "\"" "\\\"" >> sprintf "\"%s\""
    let quotePath (path: Path) =
        path |> string |> quote
    let getPath (item: Item) =
        { Value = item.Path |> quotePath; PathsUsed = [item.Path] }
    let getPaths (separator: string option) (items: Item seq) =
        let paths = items |> Seq.map (fun item -> item.Path) |> Seq.toList
        paths
        |> Seq.map quotePath
        |> String.concat (separator |? " ")
        |> Option.ofCond String.isNotEmpty
        |> Option.map (fun value -> { Value = value; PathsUsed = paths })
    let withNoPathsUsed value =
        { Value = value; PathsUsed = [] }
    let substringTo index (str: String) = str |> String.substring 0 index
    let substringAfter index (str: String) = str |> String.substringFrom (index + 1)
    let removeIndex index (str: String) = (substringTo index str) + (substringAfter index str)
    let replaceVariable (varMatch: VariableMatch) (varApplication: VariableApplication) =
        let applyVar var =
            match var with
            | SelectedItems separator ->
                Ok (model.ActionItems |> getPaths separator)
            | SelectedFiles separator ->
                Ok (model.ActionItems |> Seq.filter isFile |> getPaths separator)
            | SelectedFolders separator ->
                Ok (model.ActionItems |> Seq.filter isDirectory |> getPaths separator)
            | CursorItem ->
                Ok (model.CursorItem |> Option.map getPath)
            | CursorFile ->
                Ok (model.CursorItem |> Option.filter isFile |> Option.map getPath)
            | CursorFolder ->
                Ok (model.CursorItem |> Option.filter isDirectory |> Option.map getPath)
            | Location ->
                Ok (Some (model.Location |> quotePath |> withNoPathsUsed))
            | GitRoot ->
                gitRoot fs model.Location
                |> Result.map (Option.map (quotePath >> withNoPathsUsed))
                |> Result.mapError (fun e -> MainStatus.CouldNotExecute ("find git root folder", e))
            | Env variableName ->
                Ok (getEnvVar variableName |> Option.map (quote >> withNoPathsUsed))
        varMatch.Variables
        |> Seq.fold
            (fun application var ->
                match application with
                | Ok None -> applyVar var
                | _ -> application
            )
            (Ok None)
        |> Result.bind (fun application ->
            let createApplication newPaths valueParts =
                Ok {
                    Value = valueParts |> String.concat String.Empty
                    PathsUsed = newPaths @ varApplication.PathsUsed
                }
            match application, varMatch.BracketIndexes with
            | None, None ->
                Error (MainStatus.ToolVariableRequired (toolName, varMatch.Variables |> List.last))
            | None, Some (bracketStart, bracketEnd) ->
                createApplication [] [
                    varApplication.Value |> substringTo bracketStart
                    varApplication.Value |> substringAfter bracketEnd
                ]
            | Some valueApplication, None ->
                createApplication valueApplication.PathsUsed [
                    varApplication.Value |> substringTo varMatch.StartIndex
                    valueApplication.Value
                    varApplication.Value |> substringAfter varMatch.EndIndex
                ]
            | Some valueApplication, Some (bracketStart, bracketEnd) ->
                createApplication valueApplication.PathsUsed [
                    varApplication.Value |> substringTo varMatch.StartIndex |> removeIndex bracketStart
                    valueApplication.Value
                    varApplication.Value |> removeIndex bracketEnd |> substringAfter varMatch.EndIndex
                ]
        )
    variableRegex.Matches(str)
    |> Seq.cast<Match>
    |> Seq.choose (parseVariable str)
    |> Seq.rev
    |> Seq.fold
        (fun applicationResult var -> applicationResult |> Result.bind (replaceVariable var))
        (Ok { Value = str; PathsUsed = [] })

let executeTool (os: IOperatingSystem) (fs: IFileSystemReader) toolName (model: MainModel) = result {
    let! tool = model.Config.FindTool toolName |> Result.ofOption (MainStatus.ToolDoesNotExist toolName)
    let! args = tool.Arguments |> substituteVariables fs os.GetEnvironmentVariable tool.ToolName model
    do! os.Execute tool.HideWindow model.Location tool.Exe args.Value
        |> Result.mapError (fun e -> MainStatus.CouldNotExecute ("Tool - " + tool.ToolName, e))
    return
        model
        |> MainModel.mapHistory (History.withFilePaths model.Config.Limits.PathHistory args.PathsUsed)
        |> MainModel.withMessage (MainStatus.ExecutedTool tool.ToolName)
}
