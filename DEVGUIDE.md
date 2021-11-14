# Development Guide
The purpose of this document is to orient you in the Koffee codebase and to help you learn to effectively modify it.

## Setup
1. Install the build tools listed in the ["From Source" installation instructions in the Readme](README.md#from-source).
2. For the best experience, use Visual Studio Code with the Ionide extension or Visual Studio

## Architecture
Koffee is built on [VinylUI](https://github.com/Acadian-Ambulance/vinyl-ui), so you should familiarize yourself with
that library first. Reading the ReadMe should give you a decent idea of the overall structure of the UI framework.

The bulk of Koffee's code is for the Main window, which is split into MainModel, MainView and MainLogic.
- MainModel.fs contains all of the domain types for the Main window. This includes the model for the state of the window
    (`MainModel`) as well as the logical events (`MainEvents`).
- MainView.fs contains the bindings between the controls and the model (`binder`) and event mapping from control events
    to logical events (`events`). It also has functions for the status and error messages
- MainLogic.fs contains the controller, lower-level event handlers (key presses, input, etc), and the `dispatcher`
    function that maps events to event handlers. Most of the event handlers are organized into sub-modules:
    - `Main.Nav`: Opening folders and navigating paths
    - `Main.Search`: Find and Search handlers
    - `Main.Action`: Actions on file system items (create, move, copy, etc.)
    - `Main.Command`: Other commands such as opening items, settings, external tools, drag'n'drop and anything else that
        doesn't fit in the above categories

For instance, adding a new key-bindable command typically involves:
1. Adding an event type to MainEvents discriminated union and adding it to the Bindable list
2. Adding a keybinding in KeyBinding.fs
3. If a new piece of state is needed, add a property to MainModel
4. Adding an event handler function to the appropriate Main sub-module and adding a case to map the event to the new
    function in the dispatcher function

Still have questions? Open an issue or [email me](mailto:miller.mattster@gmail.com) and we can discuss it!
