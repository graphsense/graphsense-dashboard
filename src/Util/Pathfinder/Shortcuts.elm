module Util.Pathfinder.Shortcuts exposing
    ( Shortcut
    , all
    , chord
    , deleteSelection
    , export
    , hintDelayMs
    , modKeyLabel
    , open
    , redo
    , save
    , undo
    )

{-| The Pathfinder's keyboard shortcuts, in one place, so the toolbar tooltips
and the shortcut hint overlay cannot drift apart from each other or from the
handlers in `Sub.Pathfinder` and `main.js`.

`label` is a translation key.

-}


type alias Shortcut =
    { keys : List String
    , withMod : Bool
    , label : String
    }


{-| How long Ctrl/Cmd has to be held before the hint overlay shows. Long enough
that an ordinary chord (Ctrl+S) or a Ctrl+click never sees it.
-}
hintDelayMs : Float
hintDelayMs =
    600


addAddress : Shortcut
addAddress =
    Shortcut [ "K" ] True "Add address or tx"


findOnGraph : Shortcut
findOnGraph =
    Shortcut [ "F" ] True "Find on graph"


save : Shortcut
save =
    Shortcut [ "S" ] True "save file"


open : Shortcut
open =
    Shortcut [ "O" ] True "open"


export : Shortcut
export =
    Shortcut [ "E" ] True "export graph"


undo : Shortcut
undo =
    Shortcut [ "Z" ] True "Undo"


redo : Shortcut
redo =
    Shortcut [ "Y" ] True "Redo"


selectAll : Shortcut
selectAll =
    Shortcut [ "A" ] True "Select all"


deleteSelection : Shortcut
deleteSelection =
    Shortcut [ "Del" ] False "Delete selection"


navigate : Shortcut
navigate =
    Shortcut [ "←", "→", "↑", "↓" ] False "Move the selection along the graph"


escape : Shortcut
escape =
    Shortcut [ "Esc" ] False "Close search or dialog"


{-| In the order the overlay lists them.
-}
all : List Shortcut
all =
    [ addAddress
    , findOnGraph
    , save
    , open
    , export
    , undo
    , redo
    , selectAll
    , deleteSelection
    , navigate
    , escape
    ]


modKeyLabel : Bool -> String
modKeyLabel isMac =
    if isMac then
        "⌘"

    else
        "Ctrl"


{-| The chord as a tooltip writes it: `Ctrl+S`, or `⌘S` on macOS.
-}
chord : Bool -> Shortcut -> String
chord isMac shortcut =
    let
        keys =
            String.join " " shortcut.keys
    in
    if not shortcut.withMod then
        keys

    else if isMac then
        modKeyLabel isMac ++ keys

    else
        modKeyLabel isMac ++ "+" ++ keys
