module Iknaio.DesignToken exposing (..)

import Basics.Extra exposing (uncurry)
import Css exposing (property)
import Dict exposing (Dict)
import Tuple exposing (first)


type alias DesignToken =
    { name : String
    , light : String
    , dark : String
    }


variables : Bool -> List DesignToken -> Css.Style
variables lightmode =
    let
        mode =
            if lightmode then
                .light

            else
                .dark
    in
    List.map (\dt -> mode dt |> property ("--" ++ dt.name))
        >> Css.batch


type alias Style =
    { tokens : Dict String DesignToken
    , duration : Int
    }


init : Style
init =
    { tokens = Dict.empty
    , duration = 0
    }


token : String -> DesignToken -> Style -> Style
token name dt s =
    { s | tokens = Dict.insert name dt s.tokens }


withDuration : Int -> Style -> Style
withDuration duration s =
    { s | duration = duration }


css : Style -> Css.Style
css s =
    let
        nameToDuration name =
            name ++ " " ++ String.fromInt s.duration ++ "ms"

        tokens =
            s.tokens
                |> Dict.toList

        transitions =
            if s.duration == 0 then
                []

            else
                tokens
                    |> List.map (first >> nameToDuration)
                    |> String.join ","
                    |> property "transition"
                    |> List.singleton
    in
    tokens
        |> List.map (uncurry toProperty)
        |> List.append transitions
        |> Css.batch


toProperty : String -> DesignToken -> Css.Style
toProperty name dt =
    "var(--"
        ++ dt.name
        ++ ")"
        |> property name
