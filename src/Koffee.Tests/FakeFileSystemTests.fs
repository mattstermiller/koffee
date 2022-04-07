module Koffee.FakeFileSystemTests

open NUnit.Framework
open FsUnitTyped

let createFs () =
    FakeFileSystem [
        folder "programs" [
            file "koffee.exe"
            file "notepad.exe"
        ]
        file "readme.md"
    ]

[<Test>]
let ``GetItem returns items`` () =
    let fs = createFs ()
    fs.AddExn false (exn "don't throw") "/c/readme.md"
    let file = createFile "/c/programs/koffee.exe"
    fs.GetItem file.Path |> shouldEqual (Ok (Some file))
    let folder = createFolder "/c/programs"
    fs.GetItem folder.Path |> shouldEqual (Ok (Some folder))

[<Test>]
let ``GetItem on exn path throws exn`` () =
    let fs = createFs ()
    let path = createPath "/c/programs"
    fs.AddExnPath false ex path
    fs.GetItem path |> shouldEqual (Error ex)

[<Test>]
let ``GetItem on path with writeOnly exn and other exn throws once`` () =
    let fs = createFs ()
    let path = createPath "/c/programs"
    fs.AddExnPath true (exn "write error") path
    fs.AddExnPath false ex path
    [fs.GetItem path; fs.GetItem path]
    |> shouldEqual [Error ex; Ok (Some (createFolder "/c/programs"))]

[<Test>]
let ``GetItems on folder returns items`` () =
    let fs = createFs ()
    fs.GetItems (createPath "/c") |> shouldEqual (Ok [
        createFolder "/c/programs"
        createFile "/c/readme.md"
    ])

[<Test>]
let ``GetItems on exn path throws exn`` () =
    let fs = createFs ()
    let path = createPath "/c/programs"
    fs.AddExnPath false ex path
    fs.GetItems path |> shouldEqual (Error ex)

[<Test>]
let ``GetFolders returns only folders`` () =
    let fs = createFs ()
    fs.GetFolders (createPath "/c") |> shouldEqual (Ok [
        createFolder "/c/programs"
    ])

[<Test>]
let ``Create adds item`` () =
    let fs = createFs ()
    fs.Create File (createPath "/c/notes.txt") |> shouldEqual (Ok ())
    fs.ItemsShouldEqual [
        folder "programs" [
            file "koffee.exe"
            file "notepad.exe"
        ]
        file "readme.md"
        file "notes.txt"
    ]

[<Test>]
let ``Move file moves it`` () =
    let fs = createFs ()
    fs.Move (createPath "/c/readme.md") (createPath "/c/programs/docs.md") |> shouldEqual (Ok ())
    fs.ItemsShouldEqual [
        folder "programs" [
            file "docs.md"
            file "koffee.exe"
            file "notepad.exe"
        ]
    ]

[<Test>]
let ``Move file to folder that does not exist returns error`` () =
    let fs = createFs ()
    let badPath = createPath "/c/unicorn"
    fs.Move (createPath "/c/readme.md") (badPath.Join "readme.md")
    |> assertErrorExn (FakeFileSystemErrors.destPathParentDoesNotExist badPath)

[<Test>]
let ``Move file to path that is a file returns error`` () =
    let fs = createFs ()
    let badPath = createPath "/c/programs/koffee.exe"
    fs.Move (createPath "/c/readme.md") (badPath.Join "readme.md")
    |> assertErrorExn (FakeFileSystemErrors.destPathParentIsNotFolder badPath)

[<Test>]
let ``Move file to add suffix renames it`` () =
    let fs = createFs ()
    fs.Move (createPath "/c/readme.md") (createPath "/c/readme.md.bak") |> shouldEqual (Ok ())
    fs.ItemsShouldEqual [
        folder "programs" [
            file "koffee.exe"
            file "notepad.exe"
        ]
        file "readme.md.bak"
    ]

[<Test>]
let ``Move folder moves it and sub items recursively`` () =
    let fs =
        FakeFileSystem [
            folder "dest" []
            folder "stuff" [
                folder "sub" [
                    file "sub1"
                    file "sub2"
                ]
                file "file1"
                file "file2"
            ]
            file "other"
        ]
    fs.Move (createPath "/c/stuff") (createPath "/c/dest/stuff") |> shouldEqual (Ok ())
    fs.ItemsShouldEqual [
        folder "dest" [
            folder "stuff" [
                folder "sub" [
                    file "sub1"
                    file "sub2"
                ]
                file "file1"
                file "file2"
            ]
        ]
        file "other"
    ]

let ``Move folder where dest exists merges contents`` () =
    let fs =
        FakeFileSystem [
            folder "dest" [
                folder "stuff" [
                    folder "sub" [
                        file "sub existing"
                    ]
                    file "existing"
                ]
            ]
            folder "stuff" [
                folder "sub" [
                    file "sub1"
                    file "sub2"
                ]
                file "file1"
                file "file2"
            ]
            file "other"
        ]
    fs.Move (createPath "/c/stuff") (createPath "/c/dest/stuff") |> shouldEqual (Ok ())
    fs.ItemsShouldEqual [
        folder "dest" [
            folder "stuff" [
                folder "sub" [
                    file "sub existing"
                    file "sub1"
                    file "sub2"
                ]
                file "existing"
                file "file1"
                file "file2"
            ]
        ]
        file "other"
    ]

[<Test>]
let ``Create exn path does not create`` () =
    let fs = createFs ()
    let path = createPath "/c/new"
    fs.AddExnPath false ex path
    fs.Create File path |> shouldEqual (Error ex)
    fs.Items |> shouldEqual (createFs().Items)

[<TestCase(false)>]
[<TestCase(true)>]
let ``Move exn path does not move`` (errorDest: bool) =
    let fs = createFs ()
    let src = createPath "/c/readme.md"
    let dest = createPath "/c/programs/docs.md"
    fs.AddExnPath false ex (if errorDest then dest else src)
    fs.Move src dest |> shouldEqual (Error ex)
    fs.Items |> shouldEqual (createFs().Items)

[<Test>]
let ``Delete file removes it`` () =
    let fs = createFs()
    fs.Delete (createPath "/c/programs/notepad.exe") |> shouldEqual (Ok ())
    fs.ItemsShouldEqual [
        folder "programs" [
            file "koffee.exe"
        ]
        file "readme.md"
    ]

[<Test>]
let ``Delete folder removes it and contents`` () =
    let fs = createFs()
    fs.Delete (createPath "/c/programs") |> shouldEqual (Ok ())
    fs.ItemsShouldEqual [file "readme.md"]

[<TestCase(false)>]
[<TestCase(true)>]
let ``Delete exn path throws once`` writeOnlyExn =
    let fs = createFs()
    let path = createPath "/c/readme.md"
    fs.AddExnPath writeOnlyExn ex path
    [fs.Delete path; fs.Delete path] |> shouldEqual [Error ex; Ok ()]

[<Test>]
let ``GetItem then Delete with writeOnly exn then read exn throws read exn then writeOnly exn`` () =
    let fs = createFs()
    let path = createPath "/c/readme.md"
    let readExn = exn "read error"
    let writeExn = exn "write error"
    fs.AddExnPath true writeExn path
    fs.AddExnPath false readExn path
    [fs.GetItem path |> Result.map ignore; fs.Delete path] |> shouldEqual [Error readExn; Error writeExn]
