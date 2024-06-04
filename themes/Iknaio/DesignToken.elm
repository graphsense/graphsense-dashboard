module Iknaio.DesignToken exposing (..)

import Basics.Extra exposing (uncurry)
import Css exposing (property)
import Dict exposing (Dict)
import Tuple exposing (first)


type Variable
    = Variable String String


type alias DesignToken =
    { name : String
    , light : Variable
    , dark : Variable
    }


variables : Bool -> List DesignToken -> Css.Style
variables lightmode tokens =
    let
        mode =
            if lightmode then
                .light

            else
                .dark

        fold : Variable -> Dict String String -> Dict String String
        fold (Variable name value) =
            Dict.insert name value

        variables_ =
            tokens
                |> List.map mode
                |> List.foldl fold Dict.empty
                |> Dict.toList
                |> List.map (\( name, value ) -> property ("--" ++ name) value)
    in
    variables_
        ++ List.map (\dt -> mode dt |> variableReference |> property ("--" ++ dt.name)) tokens
        |> Css.batch


variableReference : Variable -> String
variableReference (Variable name _) =
    "var(--" ++ name ++ ")"


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
    toVariable dt
        |> property name


toVariable : DesignToken -> String
toVariable dt =
    "var(--"
        ++ dt.name
        ++ ")"
