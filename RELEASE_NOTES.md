# Release Notes

### 0.6.0  Multi-select (WIP)
- Changes UI to use new dark theme
- Adds "Open File and Exit" command
- Swaps key bindings for create file, create folder - `o` is now create folder
- Changes Escape key in path box to always switch focus back to item grid and resets it to current location
- Changes Tab key in path box to always move cursor to end
- #80 Fixes the Append to Name and Replace Name renaming commands to no longer use extension logic on folders
- #82 Fixes focus being lost after clicking grid item and opening folder
- Fixes undo/redo actions that error not being removed, blocking all other undo/redo actions
- Fixes double ending slash in unix drive path formatting
- Upgrades project to .NET Core SDK format. The `dotnet` command works in the repository folder to build, test, run, etc.

### 0.5.2  (2020-01-17)
- #76 Fixes error when deleting the last item in a folder

### 0.5.1  (2019-11-15)
- #75 Fixes crash on startup when the Koffee AppData folder does not exist

### 0.5.0  Recursive Search (2019-11-11)
- #54 Adds path suggestions as you type in the path box
- #51 Changes Find to a "Starts with" jump
- #73 Changes Search to filter as you type, moves case-sensitivity setting to search panel, makes Regex searching
  optional (default is now space-separated term matching)
- #23 Adds recursive sub-folder searching
- #22 Adds search history
- #74 Adds command to clear yank register
- Fixes deadlock/hang when rapidly moving or resizing the window multiple times
- Replaces configuration back-end, changing format to JSON and eliminating file load latency

### 0.4.2  (2019-06-20)
- Adds "Open With" command
- Adds "Open Properties" command
- Adds "Copy to Clipboard" command
- Adds "Start Create Shortcut to Item" command
- Adds ability to follow shortcuts to folders
- Adds ability to go back to previous path when opening to a default path
- Adds a trailing slash to path in path box
- Adds message and logging for non-crash errors
- Changes sorting to keep the same item selected
- Changes pressing Escape in path box to deselect when text is selected
- Refactored codebase to switch framework from FSharp.Desktop.UI to VinylUI
- Fixes moving folders across volumes
- Fixes opening new window to a drive root not opening correctly
- Fixes various issues when a path is provided in arguments
- Fixes path box to scroll horizontally instead of wrapping
- Fixes copy merge prompt message

### 0.4.1  Fixes and Refinements (2018-06-13)
- Adds item count and total size of files in status bar
- Adds status message when removing remembered network hosts
- Adds trailing slash on folders
- Adds Modified times to folders (Thanks Alex)
- Changes Modified times to show seconds
- Adds "B" to sizes to denote bytes (Thanks Alex)
- Adds sort indicator to column headers
- Adds pending key status message
- Adds crash logging and friendly error message
- Adds version number to title bar
- Cascades window position when opening multiple instances
- Double-clicking an item now opens it (Thanks Alex)
- Major refactors to improve error handling
- Fixes Ctrl + C crash
- Fixes wrong error message when attempting to recycle an item on a drive with no Recycle Bin
- Fixes extra config saves that could revert the yank register when switching between multiple windows
- Fixes display of underscores in status messages
- Fixes overwrite prompt not clearing previous input text
- Fixes path entry when entered path is same as current

### 0.4.0  Network and Multi-Window (2017-11-14)
- Adds support for network share and path navigation
- Adds New Window command
- Adds setting for commandline tool path
- Adds separate Find command for case-sensitivity
- Adds search setting and switches for case-sensitivity
- Adds confirmation prompt before overwriting bookmarks
- Adds setting to refresh window on focus
- Move/copy item buffer is now saved to config and shared across instances
- Fixes renaming to the same name with only casing differences
- Fixes create/rename/delete actions to do nothing on item types they can't apply to

### 0.3.1  (2017-09-24)
- Adds Settings button
- Fixed moving a folder reporting a false error message and not refreshing
- Fixed delete key not working in command input box
- Fixed opening Windows Explorer in an empty folder opening parent instead
- Fixed working directory for processes for opened files 
- Fixed undo move checking the wrong path for existing file
- Fixed create and rename overwriting existing files, now they give error message
- Fixed minimizing resets remembered window position and maximized state

### 0.3.0  Settings and Bookmarks (2017-08-07)
- Added ability to set, go to, and delete bookmarked locations
- Added sort by name, size, or modified ascending/descending
- Added Open With Text Editor command and setting for text editor path
- Added settings for startup path, path format, showing hidden files
- Added color for folders and hidden items
- Added search highlighting and match count
- Added date and size comparison for file overwrite prompt
- Replaced Rename command "replace extension" with "replace full name"
- Window size and position are now remembered
- Escape and Exit command now work when the path text box is focused
- Fixes crash when opening a file fails
- Fixes showing wrong name in move/copy overwrite prompt
- Fixes overwriting for move
- Fixes overwriting readonly and hidden files

### 0.2.1  (2017-03-18)
- Added Open Commandline
- Path box now replaces "~" with user's directory
- Window title now shows the current folder
- Move, copy and delete operations now show busy indication
- Fix crash when attempting to rename extension on items without an extension
- Fix unauthorized error when attempting to delete read-only items
- Fix empty item buffer appearing after executing a command
- Fix margins of path text box and command panel

### 0.2.0  File & Folder Manipulation (2017-01-28)
- Create, delete, rename, move, and copy files and folders
- Undo/redo of file and folder actions
- Back/forward navigation
- When navigating to parent folder, folder you were just in is selected
- Support for opening a path passed in as an argument to the executable
- Added icon and simple thematic elements

### 0.1.0  Initial Release (2016-10-22)
- Local file system navigation with VIM hotkeys
- Find by first character and search with regular expression in current folder
- Toggle between Windows and Unix-style path formats
