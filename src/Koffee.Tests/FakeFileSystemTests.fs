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
let ``Creating with drives creates correct paths`` () =
    let fs = FakeFileSystem [
        drive 'c' [
            folder "folder" [
                file "file"
            ]
        ]
        drive 'd' [
            folder "backup" []
        ]
    ]
    fs.ItemsShouldEqualList [
        Item.Basic (createPath "/c") "C" Drive
        createFolder "/c/folder"
        createFile "/c/folder/file"
        Item.Basic (createPath "/d") "D" Drive
        createFolder "/d/backup"
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
    fs.Move File (createPath "/c/readme.md") (createPath "/c/programs/docs.md") |> shouldEqual (Ok ())
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
    fs.Move File (createPath "/c/readme.md") (badPath.Join "readme.md")
    |> assertErrorExn (FakeFileSystemErrors.destPathParentDoesNotExist badPath)

[<Test>]
let ``Move file to path that is a file returns error`` () =
    let fs = createFs ()
    let badPath = createPath "/c/programs/koffee.exe"
    fs.Move File (createPath "/c/readme.md") (badPath.Join "readme.md")
    |> assertErrorExn (FakeFileSystemErrors.destPathParentIsNotFolder badPath)

[<TestCase(false)>]
[<TestCase(true)>]
let ``Move file to or from exn path returns error`` isDestExn =
    let fs = createFs ()
    let src = createPath "/c/readme.md"
    let dest = createPath "/c/programs/readme.md"
    fs.AddExnPath true ex (if isDestExn then dest else src)
    fs.Move File src dest |> assertErrorExn ex
    fs.ItemsShouldEqualList (createFs().Items)


[<Test>]
let ``Move file to add suffix renames it`` () =
    let fs = createFs ()
    fs.Move File (createPath "/c/readme.md") (createPath "/c/readme.md.bak") |> shouldEqual (Ok ())
    fs.ItemsShouldEqual [
        folder "programs" [
            file "koffee.exe"
            file "notepad.exe"
        ]
        file "readme.md.bak"
    ]

[<Test>]
let ``Move folder on same drive moves it and sub items recursively`` () =
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
    fs.Move Folder (createPath "/c/stuff") (createPath "/c/dest/stuff") |> shouldEqual (Ok ())
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

[<Test>]
let ``Move empty folder across drives moves it`` () =
    let fs =
        FakeFileSystem [
            drive 'c' [
                folder "stuff" []
            ]
            drive 'd' [
                folder "dest" [
                    file "other"
                ]
            ]
        ]
    fs.Move Folder (createPath "/c/stuff") (createPath "/d/dest/stuff") |> shouldEqual (Ok ())
    fs.ItemsShouldEqual [
        drive 'c' []
        drive 'd' [
            folder "dest" [
                folder "stuff" []
                file "other"
            ]
        ]
    ]

[<Test>]
let ``Move non-empty folder across drives returns error`` () =
    let fs =
        FakeFileSystem [
            drive 'c' [
                folder "stuff" [
                    file "file"
                ]
            ]
            drive 'd' [
                folder "dest" [
                    file "other"
                ]
            ]
        ]
    let expectedFs = fs.Items
    let src = createPath "/c/stuff"
    let dest = createPath "/d/dest/stuff"
    fs.Move Folder src dest |> assertErrorExn FakeFileSystemErrors.cannotMoveNonEmptyFolderAcrossDrives
    fs.ItemsShouldEqualList expectedFs

[<Test>]
let ``Copy file copies it`` () =
    let fs =
        FakeFileSystem [
            folder "dest" [
                file "other"
            ]
            file "file"
        ]
    fs.Copy File (createPath "/c/file") (createPath "/c/dest/file") |> shouldEqual (Ok ())
    fs.ItemsShouldEqual [
        folder "dest" [
            file "file"
            file "other"
        ]
        file "file"
    ]

[<Test>]
let ``Copy empty folder copies it`` () =
    let fs =
        FakeFileSystem [
            folder "stuff" []
            folder "dest" [
                file "other"
            ]
        ]
    fs.Copy Folder (createPath "/c/stuff") (createPath "/c/dest/stuff") |> shouldEqual (Ok ())
    fs.ItemsShouldEqual [
        folder "stuff" []
        folder "dest" [
            folder "stuff" []
            file "other"
        ]
    ]

[<Test>]
let ``Copy non-empty folder returns error`` () =
    let fs =
        FakeFileSystem [
            folder "stuff" [
                file "file"
            ]
            folder "dest" [
                file "other"
            ]
        ]
    let expectedFs = fs.Items
    let src = createPath "/c/stuff"
    let dest = createPath "/c/dest/stuff"
    fs.Copy Folder src dest |> assertErrorExn FakeFileSystemErrors.cannotCopyNonEmptyFolder
    fs.ItemsShouldEqualList expectedFs

let ``Move folder where dest exists returns error`` () =
    let fs =
        FakeFileSystem [
            folder "dest" [
                folder "stuff" []
            ]
            folder "stuff" [
                file "file1"
                file "file2"
            ]
            file "other"
        ]
    let expectedFs = fs.Items
    let src = createPath "/c/stuff"
    let dest = createPath "/c/dest/stuff"
    fs.Move Folder src dest |> assertErrorExn (FakeFileSystemErrors.destPathIsFolder dest)
    fs.ItemsShouldEqualList expectedFs

[<TestCase(false)>]
[<TestCase(true)>]
let ``Move exn path does not move`` (errorDest: bool) =
    let fs = createFs ()
    let src = createPath "/c/readme.md"
    let dest = createPath "/c/programs/docs.md"
    fs.AddExnPath false ex (if errorDest then dest else src)
    fs.Move File src dest |> shouldEqual (Error ex)
    fs.ItemsShouldEqualList (createFs().Items)

[<Test>]
let ``Create exn path does not create`` () =
    let fs = createFs ()
    let path = createPath "/c/new"
    fs.AddExnPath false ex path
    fs.Create File path |> shouldEqual (Error ex)
    fs.ItemsShouldEqualList (createFs().Items)

[<Test>]
let ``Delete file removes it`` () =
    let fs = createFs()
    fs.Delete File (createPath "/c/programs/notepad.exe") |> shouldEqual (Ok ())
    fs.ItemsShouldEqual [
        folder "programs" [
            file "koffee.exe"
        ]
        file "readme.md"
    ]

[<Test>]
let ``Delete empty folder removes it`` () =
    let fs = FakeFileSystem [
        folder "folder" []
        file "file"
    ]
    fs.Delete Folder (createPath "/c/folder") |> shouldEqual (Ok ())
    fs.ItemsShouldEqual [file "file"]

[<Test>]
let ``Delete non-empty folder returns error`` () =
    let fs = FakeFileSystem [
        folder "folder" [
            file "stuff"
        ]
        file "file"
    ]
    let expectedFs = fs.Items
    fs.Delete Folder (createPath "/c/folder") |> assertErrorExn FakeFileSystemErrors.cannotDeleteNonEmptyFolder
    fs.ItemsShouldEqualList expectedFs

[<Test>]
let ``Delete path that does not exist returns error`` () =
    let fs = createFs()
    let path = createPath "/c/secrets"
    fs.Delete File path |> assertErrorExn (FakeFileSystemErrors.pathDoesNotExist path)
    fs.ItemsShouldEqualList (createFs().Items)

[<TestCase(false)>]
[<TestCase(true)>]
let ``Delete exn path throws once`` writeOnlyExn =
    let fs = createFs()
    let path = createPath "/c/readme.md"
    fs.AddExnPath writeOnlyExn ex path
    [fs.Delete File path; fs.Delete File path] |> shouldEqual [Error ex; Ok ()]

[<Test>]
let ``GetItem then Delete with writeOnly exn then read exn throws read exn then writeOnly exn`` () =
    let fs = createFs()
    let path = createPath "/c/readme.md"
    let readExn = exn "read error"
    let writeExn = exn "write error"
    fs.AddExnPath true writeExn path
    fs.AddExnPath false readExn path
    [fs.GetItem path |> Result.map ignore; fs.Delete File path] |> shouldEqual [Error readExn; Error writeExn]
