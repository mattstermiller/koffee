[<AutoOpen>]
module Testing

open System
open FSharp.Control
open NUnit.Framework
open FsUnitTyped.TopLevelOperators
open KellermanSoftware.CompareNetObjects
open KellermanSoftware.CompareNetObjects.TypeComparers
open Acadian.FSharp
open Koffee
open Microsoft.FSharp.Reflection

let private isGeneric (genericT: Type) (t: Type) =
    t = null || t.IsGenericType && t.GetGenericTypeDefinition() = genericT

[<AbstractClass>]
type EqualityComparer() =
    inherit BaseTypeComparer(RootComparerFactory.GetRootComparer())

    override this.CompareType parms =
        if parms.Object1 <> parms.Object2 then
            this.AddDifference parms

type OptionComparer() =
    inherit EqualityComparer()

    let isOption = isGeneric typedefof<option<_>>

    override _.IsTypeMatch(type1, type2) =
        List.forall isOption [type1; type2]

type SimpleUnionComparer() =
    inherit EqualityComparer()

    let isSimpleUnion (t: Type) =
        FSharpType.IsUnion t &&
            FSharpType.GetUnionCases(t) |> Array.forall (fun case -> case.GetFields().Length = 0)

    override _.IsTypeMatch(type1, type2) =
        List.forall isSimpleUnion [type1; type2]

type FSharpListComparer() as this =
    inherit BaseTypeComparer(RootComparerFactory.GetRootComparer())

    let listComparer = ListComparer(this.RootComparer)

    let isList = isGeneric typedefof<list<_>>

    let toList source =
        let genArg = source.GetType().GetGenericArguments().[0]
        typeof<Linq.Enumerable>
            .GetMethod("ToList")
            .MakeGenericMethod(genArg)
            .Invoke(null, [|source|])

    override _.IsTypeMatch(type1, type2) =
        List.forall isList [type1; type2]

    override _.CompareType parms =
        parms.Object1 <- toList parms.Object1
        parms.Object2 <- toList parms.Object2
        listComparer.CompareType parms

type PathComparer() =
    inherit EqualityComparer()

    override _.IsTypeMatch(type1, type2) =
        List.forall ((=) typeof<Path>) [type1; type2]

let getNonFieldNames<'a> () =
    let t = typeof<'a>
    let props = t.GetProperties() |> Array.map (fun p -> p.Name)
    let fields = Reflection.FSharpType.GetRecordFields(typeof<'a>) |> Array.map (fun p -> p.Name)
    props
    |> Array.except fields
    |> Array.map ((+) (t.Name + "."))

let ignoreMembers memberNames (comparer: CompareLogic) =
    comparer.Config.MembersToIgnore.AddRange memberNames

let assertAreEqualWith (expected: 'a) (actual: 'a) comparerSetup =
    let comparer = CompareLogic()
    comparer.Config.MaxDifferences <- 10
    comparer.Config.CustomComparers.AddRange([
        OptionComparer()
        SimpleUnionComparer()
        FSharpListComparer()
        PathComparer()
    ])
    comparer.Config.MembersToIgnore.AddRange(seq {
        yield! getNonFieldNames<Item>()
        yield! getNonFieldNames<MainModel>()
        "MainModel.History"
        "MainModel.StatusHistory"
    })
    comparerSetup comparer
    let result = comparer.Compare(expected, actual)
    if not result.AreEqual then
        let comparedType = expected.GetType().Name
        let diffCount = result.Differences.Count
        let diffCountDescr =
            string diffCount +
                (if diffCount = comparer.Config.MaxDifferences then " or more" else "") +
                " difference" +
                if diffCount <> 1 then "s" else ""
        let diffs =
            result.Differences
            |> Seq.map (fun diff ->
                let childProp = if String.isNotEmpty diff.ChildPropertyName then "." + diff.ChildPropertyName else ""
                sprintf "Property: %O%O\n- Expected: %O\n- Actual: %O"
                    diff.PropertyName childProp diff.Object1Value diff.Object2Value
            )
            |> String.concat "\n"
        failwithf "%s has %s from expected value\n%s\n_____"
            comparedType diffCountDescr diffs

let assertAreEqual expected actual =
    assertAreEqualWith expected actual ignore

let assertOk res =
    match res with
    | Ok a -> a
    | Error e -> failwithf "%A" e

let seqResult handler (model: MainModel) =
    (model, handler model) ||> AsyncSeq.fold (fun m res ->
        match res with
        | Ok m -> m
        | Error e -> m.WithError e
    ) |> Async.RunSynchronously

let createPath pathStr =
    match Path.Parse pathStr with
    | Some p -> p
    | None -> failwithf "Invalid path: %s" pathStr

let createFile pathStr =
    let path = createPath pathStr
    Item.Basic path path.Name File

let createFolder pathStr =
    let path = createPath pathStr
    let name = if path.Name |> String.isNotEmpty then path.Name else string path |> String.substring 0 1
    Item.Basic path name Folder

let file name = TreeFile (name, id)
let fileWith transform name = TreeFile (name, transform)
let folder name items = TreeFolder (name, items)

let size value (item: Item) = { item with Size = Some value }
let modifiedOpt opt (item: Item) = { item with Modified = opt }
let modified value item = modifiedOpt (Some value) item
let hide value item = { item with IsHidden = value }

let sortByPath items =
    items |> List.sortBy (fun i -> i.Path |> string |> String.toLower)

type FakeFileSystem with
    member this.ItemsIn path =
        createPath path |> this.ItemsIn

    member this.Item path =
        createPath path |> this.Item

    member this.AddExn e path =
        createPath path |> this.AddExnPath e

    member this.ItemsShouldEqual expectedTree =
        let rec nestLevel level (path: Path) =
            if path.Parent = Path.Root then level else nestLevel (level+1) path.Parent
        let treeStr (items: Item list) =
            items
            |> List.map (fun item ->
                String(' ', 2 * nestLevel 0 item.Path) + item.Name + (if item.Type = Folder then "/" else "")
            )
            |> String.concat "\n"
            |> fun str -> "\n" + str + "\n"
        let expectedItems = expectedTree |> TreeItem.build |> sortByPath
        this.Items |> treeStr |> shouldEqual (expectedItems |> treeStr)
        this.Items |> assertAreEqual expectedItems

let withLocation path (model: MainModel) = model.WithLocation (createPath path)
let withBackIf condition (path, cursor) model =
    if condition then
        { model with BackStack = (path, cursor) :: model.BackStack; ForwardStack = [] }
    else
        model
let pushUndo action model = { model with UndoStack = action :: model.UndoStack }
let pushRedo action model = { model with RedoStack = action :: model.RedoStack }
let popUndo model = { model with UndoStack = model.UndoStack.Tail }
let popRedo model = { model with RedoStack = model.RedoStack.Tail }

let testModel =
    let items = [ createFile "/c/default item" ]
    let undoRedo = createFile "/c/default-undo-redo"
    { MainModel.Default with
        Directory = items
        Items = items
        BackStack = [createPath "/c/back", 8]
        ForwardStack = [createPath "/c/fwd", 9]
        UndoStack = [CreatedItem undoRedo]
        RedoStack = [RenamedItem (undoRedo, "item")]
        Config = { Config.Default with PathFormat = Unix }
    } |> withLocation "/c"

let ex = System.UnauthorizedAccessException() :> exn
