module Testing

open NUnit.Framework
open KellermanSoftware.CompareNetObjects

type ListComparer() =
    inherit TypeComparers.BaseTypeComparer(RootComparerFactory.GetRootComparer())

    override this.IsTypeMatch(type1, type2) =
        let isList (t: System.Type) =
            t = null || t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<list<_>>
        isList type1 && isList type2

    override this.CompareType parms =
        if parms.Object1 <> parms.Object2 then
            this.AddDifference parms

let assertAreEqualWith expected actual (comparer: CompareLogic) =
    comparer.Config.MaxDifferences <- 10
    comparer.Config.CustomComparers.Add(ListComparer())
    let result = comparer.Compare(expected, actual)
    Assert.IsTrue(result.AreEqual, result.DifferencesString)

let assertAreEqual expected actual =
    CompareLogic() |> assertAreEqualWith expected actual

let ignoreMembers memberNames (comparer: CompareLogic) =
    comparer.Config.MembersToIgnore.AddRange memberNames
    comparer
