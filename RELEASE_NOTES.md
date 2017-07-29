# Release Notes

### v0.3.0  Settings and Bookmarks
- Added ability to set, go to, and delete bookmarked locations
- Added sort by name, size, or modified ascending/descending
- Added Open With Text Editor command and setting for text editor path
- Added settings for startup path, path format, showing hidden files
- Added color for folders and hidden items
- Added search highlighting and match count
- Added date and size comparison for file overwrite prompt
- Window size and position are now remembered
- Escape and Exit command now work when the path text box is focused
- Fixes crash when opening a file fails
- Fixes showing wrong name in move/copy overwrite prompt
- Fixes overwriting for move
- Fixes overwriting readonly and hidden files

### v0.2.1
- Added Open Commandline
- Path box now replaces "~" with user's directory
- Window title now shows the current folder
- Move, copy and delete operations now show busy indication
- Fix crash when attempting to rename extension on items without an extension
- Fix unauthorized error when attempting to delete read-only items
- Fix empty item buffer appearing after executing a command
- Fix margins of path text box and command panel

### v0.2.0  File & Folder Manipulation
- Create, delete, rename, move, and copy files and folders
- Undo/redo of file and folder actions
- Back/forward navigation
- When navigating to parent folder, folder you were just in is selected
- Support for opening a path passed in as an argument to the executable
- Added icon and simple thematic elements

### v0.1.0  Initial Release
- Local file system navigation with VIM hotkeys
- Find by first character and search with regular expression in current folder
- Toggle between Windows and Unix-style path formats
