module Model.ThemeMode exposing (ThemeMode(..), all, fromString, isLight, toString)

{-| Which colour scheme the user asked for. `System` follows the browser's
`prefers-color-scheme`, the other two override it.

The resolved value is `Config.View.lightmode`, kept as a plain `Bool` because
views and plugins only ever ask "light or dark?".

-}


type ThemeMode
    = System
    | Light
    | Dark


all : List ThemeMode
all =
    [ System, Light, Dark ]


{-| Whether the UI renders light, given the browser's `prefers-color-scheme: dark`.
-}
isLight : Bool -> ThemeMode -> Bool
isLight systemDark mode =
    case mode of
        System ->
            not systemDark

        Light ->
            True

        Dark ->
            False


{-| The value stored in localStorage under `themeMode`. `index.html` reads the
same strings before Elm starts, so keep the two in sync.
-}
toString : ThemeMode -> String
toString mode =
    case mode of
        System ->
            "system"

        Light ->
            "light"

        Dark ->
            "dark"


fromString : String -> Maybe ThemeMode
fromString s =
    case s of
        "system" ->
            Just System

        "light" ->
            Just Light

        "dark" ->
            Just Dark

        _ ->
            Nothing
