module Testing

open System
open NUnit.Framework
open KellermanSoftware.CompareNetObjects
open KellermanSoftware.CompareNetObjects.TypeComparers
open FSharp.Desktop.UI
open Koffee
open ModelExtensions
open Foq

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

let createPath pathStr = (Path.Parse ("/c/" + pathStr)).Value

let createNode path name =
    { Path = sprintf "%s/%s" path name |> createPath; Name = name; Type = Folder;
      Modified = None; Size = None; IsHidden = false; IsSearchMatch = false }

let createBaseTestModel() =
    let model = Model.Create<MainModel>()
    model.BackStack <- [createPath "back", 8]
    model.ForwardStack <- [createPath "fwd", 9]
    let node = createNode "path" "default undo-redo"
    model.UndoStack <- [CreatedItem node]
    model.RedoStack <- [RenamedItem (node, "item")]
    model.PathFormat <- Unix
    // simulate grid losing selected item (bound to cursor) when data source changes
    model.OnPropertyChanged <@ model.Nodes @> (fun _ -> model.Cursor <- -1)
    model

let baseFileSysMock (newNodes: Node list) =
    let path =
        match newNodes with
        | node :: _ -> node.Path.Parent
        | [] -> createPath "path"
    Mock<IFileSystemService>()
        .Setup(fun x -> <@ x.GetNodes (any()) path @>).Returns(newNodes)
