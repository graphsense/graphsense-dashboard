module Locale.View exposing (string, text)

import Css exposing (num, opacity)
import Css.Transitions as T exposing (transition)
import Dict
import Ease
import Html.Styled exposing (Html, span, text)
import Html.Styled.Attributes exposing (css)
import Locale.Model exposing (Model, State(..))
import Locale.Update exposing (duration)


string : Model -> String -> String
string model key =
    let
        lower =
            String.toLower key
                |> Debug.log "lower"

        raise s =
            if String.left 1 key /= String.left 1 lower then
                (String.left 1 s
                    |> String.toUpper
                )
                    ++ String.dropLeft 1 s

            else
                s

        fix =
            Dict.get lower
                >> Maybe.withDefault key
                >> raise
    in
    case model.mapping of
        Empty ->
            raise key

        Settled mapping ->
            fix mapping

        Transition start end delta ->
            delta
                / duration
                |> Ease.inOutQuad
                |> mix (fix start) (fix end)


mix : String -> String -> Float -> String
mix start end progress =
    let
        step =
            max
                (String.length start)
                (String.length end)
                |> toFloat
                |> (*) progress
                |> round
    in
    String.left step end
        ++ String.dropLeft step start


text : Model -> String -> Html msg
text model key =
    let
        fade =
            model.mapping == Empty
    in
    span
        [ css
            [ opacity <|
                num <|
                    if fade then
                        0.5

                    else
                        1
            , transition
                [ T.opacity 500
                ]
            ]
        ]
        [ string model key
            |> Html.Styled.text
        ]
