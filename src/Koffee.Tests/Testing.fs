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

type StructuralEqualityComparer() =
    inherit BaseTypeComparer(RootComparerFactory.GetRootComparer())

    override this.IsTypeMatch(type1, type2) =
        let isGeneric (genericT: Type) (t: Type) =
            t = null || t.IsGenericType && t.GetGenericTypeDefinition() = genericT
        let areBoth t = isGeneric t type1 && isGeneric t type2
        [typedefof<list<_>>; typedefof<option<_>>]
            |> List.exists areBoth

    override this.CompareType parms =
        if parms.Object1 <> parms.Object2 then
            this.AddDifference parms

type PathComparer() =
    inherit BaseTypeComparer(RootComparerFactory.GetRootComparer())

    override this.IsTypeMatch(type1, type2) =
        type1 = typeof<Path> && type2 = typeof<Path>

    override this.CompareType parms =
        if parms.Object1 <> parms.Object2 then
            this.AddDifference parms

let getNonFieldNames<'a> () =
    let t = typeof<'a>
    let props = t.GetProperties() |> Array.map (fun p -> p.Name)
    let fields = Reflection.FSharpType.GetRecordFields(typeof<'a>) |> Array.map (fun p -> p.Name)
    props |> Array.except fields

let ignoreMembers memberNames (comparer: CompareLogic) =
    comparer.Config.MembersToIgnore.AddRange memberNames

let assertAreEqualWith (expected: 'a) (actual: 'a) comparerSetup =
    let comparer = CompareLogic()
    comparer.Config.MaxDifferences <- 10
    comparer.Config.CustomComparers.AddRange([StructuralEqualityComparer(); PathComparer()])
    comparer.Config.MembersToIgnore.AddRange((getNonFieldNames<MainModel>() |> Seq.toList) @ ["History"])
    comparerSetup comparer
    let result = comparer.Compare(expected, actual)
    Assert.IsTrue(result.AreEqual, result.DifferencesString)

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

    member this.ItemsShouldEqual items =
        this.Items |> shouldEqual (items |> TreeItem.build |> sortByPath)

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
