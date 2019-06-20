[<AutoOpen>]
module Testing

open System
open FSharp.Control
open NUnit.Framework
open KellermanSoftware.CompareNetObjects
open KellermanSoftware.CompareNetObjects.TypeComparers
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

let ignoreMembers memberNames (comparer: CompareLogic) =
    comparer.Config.MembersToIgnore.AddRange memberNames

let assertAreEqualWith (expected: 'a) (actual: 'a) comparerSetup =
    let comparer = CompareLogic() 
    comparer.Config.MaxDifferences <- 10
    comparer.Config.CustomComparers.Add(StructuralEqualityComparer())
    let fields = Reflection.FSharpType.GetRecordFields(typeof<MainModel>) |> Seq.map (fun p -> p.Name)
    comparer.Config.MembersToInclude.AddRange(fields)
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

let createPath pathStr = (Path.Parse pathStr).Value

let createNode pathStr =
    let path = createPath pathStr
    { Path = path; Name = path.Name; Type = Folder;
      Modified = None; Size = None; IsHidden = false; IsSearchMatch = false }

let baseModel =
    let node = createNode "/c/path/default undo-redo"
    { MainModel.Default with
        BackStack = [createPath "/c/back", 8]
        ForwardStack = [createPath "/c/fwd", 9]
        UndoStack = [CreatedItem node]
        RedoStack = [RenamedItem (node, "item")]
        PathFormat = Unix
    }
