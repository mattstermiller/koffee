# Release Notes

### 0.4.0  Network and Multi-Window
- Adds support for network share and path navigation
- Adds New Window command
- Adds setting for commandline tool path
- Adds separate Find command for case-sensitivity
- Adds search setting and switches for case-sensitivity
- Adds confirmation prompt before overwriting bookmarks
- Adds setting to refresh window on focus
- Move/copy item buffer is now saved to config and shared across instances
- Fixes renaming to the same name with only casing differences
- Fixes create/rename/delete actions to do nothing on node types they can't apply to

### 0.3.1
- Adds Settings button
- Fixed moving a folder reporting a false error message and not refreshing
- Fixed delete key not working in command input box
- Fixed opening Windows Explorer in an empty folder opening parent instead
- Fixed working directory for processes for opened files 
- Fixed undo move checking the wrong path for existing file
- Fixed create and rename overwriting existing files, now they give error message
- Fixed minimizing resets remembered window position and maximized state

### 0.3.0  Settings and Bookmarks
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

### 0.2.1
- Added Open Commandline
- Path box now replaces "~" with user's directory
- Window title now shows the current folder
- Move, copy and delete operations now show busy indication
- Fix crash when attempting to rename extension on items without an extension
- Fix unauthorized error when attempting to delete read-only items
- Fix empty item buffer appearing after executing a command
- Fix margins of path text box and command panel

### 0.2.0  File & Folder Manipulation
- Create, delete, rename, move, and copy files and folders
- Undo/redo of file and folder actions
- Back/forward navigation
- When navigating to parent folder, folder you were just in is selected
- Support for opening a path passed in as an argument to the executable
- Added icon and simple thematic elements

### 0.1.0  Initial Release
- Local file system navigation with VIM hotkeys
- Find by first character and search with regular expression in current folder
- Toggle between Windows and Unix-style path formats
