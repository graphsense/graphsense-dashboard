module Util.Flags exposing (..)

{- See https://dev.to/jorik/country-code-to-flag-emoji-a21 -}


getFlagEmoji : String -> String
getFlagEmoji =
    String.toList
        >> List.map Char.toCode
        >> List.map (\x -> 127397 + x)
        >> List.map Char.fromCode
        >> String.fromList
