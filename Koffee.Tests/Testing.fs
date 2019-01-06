module Testing

open System
open NUnit.Framework
open KellermanSoftware.CompareNetObjects
open KellermanSoftware.CompareNetObjects.TypeComparers
open FSharp.Desktop.UI
open Koffee
open ModelExtensions

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
    comparer

let assertAreEqualWith expected actual (comparer: CompareLogic) =
    comparer.Config.MaxDifferences <- 10
    comparer.Config.CustomComparers.Add(StructuralEqualityComparer())
    comparer |> ignoreMembers ["SelectedNode"] |> ignore
    let result = comparer.Compare(expected, actual)
    Assert.IsTrue(result.AreEqual, result.DifferencesString)

let assertAreEqual expected actual =
    CompareLogic() |> assertAreEqualWith expected actual

let createPath pathStr = (Path.Parse pathStr).Value

let createNode pathStr =
    let path = createPath pathStr
    { Path = path; Name = path.Name; Type = Folder;
      Modified = None; Size = None; IsHidden = false; IsSearchMatch = false }

let createBaseTestModel() =
    let model = Model.Create<MainBindModel>()
    model.BackStack <- [createPath "/c/back", 8]
    model.ForwardStack <- [createPath "/c/fwd", 9]
    let node = createNode "/c/path/default undo-redo"
    model.UndoStack <- [CreatedItem node]
    model.RedoStack <- [RenamedItem (node, "item")]
    model.PathFormat <- Unix
    // simulate grid losing selected item (bound to cursor) when data source changes
    model.OnPropertyChanged <@ model.Nodes @> (fun _ -> model.Cursor <- -1)
    model
