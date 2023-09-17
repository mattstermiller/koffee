module Koffee.FakeFileSystemTests

open NUnit.Framework
open FsUnitTyped

let createFsWith driveFunc =
    FakeFileSystem [
        driveFunc [
            folder "programs" [
                file "koffee.exe"
                file "notepad.exe"
            ]
            file "readme.md"
        ]
    ]

let createFsWithDriveSize () = createFsWith (driveWithSize 'c' 100L)
let createFs () = createFsWith (drive 'c')

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
        createDrive 'c'
        createFolder "/c/folder"
        createFile "/c/folder/file"
        createDrive 'd'
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
let ``GetItems on root returns drives`` () =
    let fs = FakeFileSystem [
        drive 'c' [
            folder "folder" [
                file "file"
            ]
        ]
        drive 'd' []
    ]
    fs.GetItems Path.Root |> shouldEqual (Ok [
        createDrive 'c'
        createDrive 'd'
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

[<TestCase("/")>]
[<TestCase("/c")>]
[<TestCase("/d")>]
[<TestCase("/e")>]
[<TestCase("/f")>]
[<TestCase("/d/file")>]
[<TestCase("/e/file")>]
[<TestCase("/f/file")>]
[<TestCase("/net")>]
[<TestCase("/net/share")>]
[<TestCase("/net/share/file")>]
let ``IsPathRecyclable returns false for drives and paths not on local drive with size`` path =
    let fs = FakeFileSystem [
        driveWithSize 'c' 100L []
        driveWithSize 'd' 0L []
        drive 'e' []
    ]
    fs.IsPathRecyclable (createPath path) |> shouldEqual false

[<TestCase("/c/file")>]
[<TestCase("/c/folder/file")>]
let ``IsPathRecyclable returns true for items on local drive with size`` path =
    let fs = FakeFileSystem [
        driveWithSize 'c' 100L []
    ]
    fs.IsPathRecyclable (createPath path) |> shouldEqual true

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
let ``Create exn path does not create`` () =
    let fs = createFs ()
    let path = createPath "/c/new"
    fs.AddExnPath false ex path
    fs.Create File path |> shouldEqual (Error ex)
    fs.ItemsShouldEqualList (createFs().Items)

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
let ``Move file to folder that does not exist returns error`` () =
    let fs = createFs ()
    let badPath = createPath "/c/unicorn"
    fs.Move File (createPath "/c/readme.md") (badPath.Join "readme.md")
    |> assertErrorExn (FakeFileSystemErrors.destPathParentDoesNotExist badPath)

[<Test>]
let ``Move file to path that is under path of an existing file returns error`` () =
    let fs = createFs ()
    let existingFilePath = createPath "/c/programs/koffee.exe"
    fs.Move File (createPath "/c/readme.md") (existingFilePath.Join "readme.md")
    |> assertErrorExn (FakeFileSystemErrors.destPathParentIsNotFolder existingFilePath)

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
let ``Move file where dest exists overwrites dest`` () =
    let fs =
        FakeFileSystem [
            folder "dest" [
                file "file"
            ]
            fileWith (size 7L) "file"
            file "other"
        ]
    let src = createPath "/c/file"
    let dest = createPath "/c/dest/file"
    fs.Move File src dest |> assertOk
    fs.ItemsShouldEqual [
        folder "dest" [
            fileWith (size 7L) "file"
        ]
        file "other"
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
let ``Move folder that is empty across drives moves it`` () =
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
let ``Move folder that is not empty across drives returns error`` () =
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
let ``Move folder where dest exists returns error`` () =
    let fs =
        FakeFileSystem [
            folder "dest" [
                folder "folder" []
            ]
            folder "folder" [
                file "file1"
                file "file2"
            ]
            file "other"
        ]
    let expectedFs = fs.Items
    let src = createPath "/c/folder"
    let dest = createPath "/c/dest/folder"
    fs.Move Folder src dest |> assertErrorExn (FakeFileSystemErrors.itemAlreadyExistsOnPath dest)
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
let ``Copy file where dest exists overwrites dest`` () =
    let fs =
        FakeFileSystem [
            folder "dest" [
                file "file"
            ]
            fileWith (size 7L) "file"
            file "other"
        ]
    let src = createPath "/c/file"
    let dest = createPath "/c/dest/file"
    fs.Copy File src dest |> assertOk
    fs.ItemsShouldEqual [
        folder "dest" [
            fileWith (size 7L) "file"
        ]
        fileWith (size 7L) "file"
        file "other"
    ]

[<Test>]
let ``Copy folder that is empty copies it`` () =
    let fs =
        FakeFileSystem [
            folder "dest" [
                file "other"
            ]
            folder "folder" []
        ]
    fs.Copy Folder (createPath "/c/folder") (createPath "/c/dest/folder") |> shouldEqual (Ok ())
    fs.ItemsShouldEqual [
        folder "dest" [
            folder "folder" []
            file "other"
        ]
        folder "folder" []
    ]

[<Test>]
let ``Copy folder that is not empty returns error`` () =
    let fs =
        FakeFileSystem [
            folder "dest" [
                file "other"
            ]
            folder "folder" [
                file "file"
            ]
        ]
    let expectedFs = fs.Items
    let src = createPath "/c/folder"
    let dest = createPath "/c/dest/folder"
    fs.Copy Folder src dest |> assertErrorExn FakeFileSystemErrors.cannotCopyNonEmptyFolder
    fs.ItemsShouldEqualList expectedFs

[<Test>]
let ``Copy folder where dest exists returns error`` () =
    let fs =
        FakeFileSystem [
            folder "dest" [
                folder "folder" []
            ]
            folder "folder" []
            file "other"
        ]
    let expectedFs = fs.Items
    let src = createPath "/c/folder"
    let dest = createPath "/c/dest/folder"
    fs.Copy Folder src dest |> assertErrorExn (FakeFileSystemErrors.itemAlreadyExistsOnPath dest)
    fs.ItemsShouldEqualList expectedFs

[<TestCase(false)>]
[<TestCase(true)>]
let ``Recycle or Delete file removes it`` isRecycle =
    let fs = createFsWithDriveSize()
    let path = "/c/programs/notepad.exe"
    let testFunc = if isRecycle then fs.Recycle 2L else fs.Delete
    testFunc File (createPath path) |> shouldEqual (Ok ())
    fs.ItemsShouldEqual [
        driveWithSize 'c' 100L [
            folder "programs" [
                file "koffee.exe"
            ]
            file "readme.md"
        ]
    ]
    fs.RecycleBin |> shouldEqual (if isRecycle then [createFile path] else [])

[<Test>]
let ``Recycle non-empty folder removes it`` () =
    let fs = createFsWithDriveSize()
    let path = "/c/programs"
    fs.Recycle 2L Folder (createPath path) |> shouldEqual (Ok ())
    fs.ItemsShouldEqual [
        driveWithSize 'c' 100L [
            file "readme.md"
        ]
    ]
    fs.RecycleBin |> shouldEqual [createFolder path]

[<Test>]
let ``Recycle item on drive with no size returns error`` () =
    let fs = createFs()
    let path = createPath "/c/programs"
    let expectedItems = fs.Items
    fs.Recycle 10L Folder path |> assertErrorExn FakeFileSystemErrors.cannotRecycleItemOnDriveWithNoSize
    fs.ItemsShouldEqualList expectedItems
    fs.RecycleBin |> shouldEqual []

[<Test>]
let ``Recycle item that is too large returns error`` () =
    let fs = createFsWithDriveSize()
    let path = createPath "/c/programs"
    let expectedItems = fs.Items
    fs.Recycle 10L Folder path |> assertErrorExn (FakeFileSystemErrors.cannotRecycleItemThatDoesNotFit 10L)
    fs.ItemsShouldEqualList expectedItems
    fs.RecycleBin |> shouldEqual []

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

[<TestCase(false)>]
[<TestCase(true)>]
let ``Recycle or Delete path that does not exist returns error`` isRecycle =
    let fs = createFsWithDriveSize()
    let path = createPath "/c/secrets"
    let expectedItems = fs.Items
    let testFunc = if isRecycle then fs.Recycle 2L else fs.Delete
    testFunc File path |> assertErrorExn (FakeFileSystemErrors.pathDoesNotExist path)
    fs.ItemsShouldEqualList expectedItems
    fs.RecycleBin |> shouldEqual []

[<TestCase(false, false)>]
[<TestCase(false, true)>]
[<TestCase(true, false)>]
[<TestCase(true, true)>]
let ``Recycle or Delete exn path throws once`` isRecycle writeOnlyExn =
    let fs = createFsWithDriveSize()
    let path = createPath "/c/readme.md"
    fs.AddExnPath writeOnlyExn ex path
    let testFunc = if isRecycle then fs.Recycle 2L else fs.Delete
    [testFunc File path; testFunc File path] |> shouldEqual [Error ex; Ok ()]
    fs.RecycleBin |> shouldEqual (if isRecycle then [createFile (string path)] else [])

[<Test>]
let ``GetItem then Delete with writeOnly exn then read exn throws read exn then writeOnly exn`` () =
    let fs = createFs()
    let path = createPath "/c/readme.md"
    let readExn = exn "read error"
    let writeExn = exn "write error"
    fs.AddExnPath true writeExn path
    fs.AddExnPath false readExn path
    (fs.GetItem path, fs.Delete File path) |> shouldEqual (Error readExn, Error writeExn)
