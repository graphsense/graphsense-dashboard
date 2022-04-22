module Locale.View exposing (float, int, percentage, string, text, timestamp)

import Css exposing (num, opacity)
import Css.Transitions as T exposing (transition)
import DateFormat exposing (..)
import Dict
import Ease
import FormatNumber
import FormatNumber.Locales
import Html.Styled exposing (Html, span, text)
import Html.Styled.Attributes exposing (css)
import Locale.Model exposing (Model, State(..))
import Locale.Update exposing (duration)
import RecordSetter exposing (..)
import Time


string : Model -> String -> String
string model key =
    let
        lower =
            String.toLower key

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


float : Model -> Float -> String
float { numberFormat } x =
    FormatNumber.format numberFormat x


int : Model -> Int -> String
int model =
    toFloat
        >> float
            (setDecimals model (FormatNumber.Locales.Exact 0))


timestamp : Model -> Int -> String
timestamp { locale, timeLang, zone } =
    let
        format =
            case locale of
                "de" ->
                    [ dayOfMonthNumber
                    , DateFormat.text ". "
                    , monthNameFull
                    , DateFormat.text " "
                    , yearNumber
                    , DateFormat.text " "
                    , hourFixed
                    , DateFormat.text ":"
                    , minuteFixed
                    ]

                _ ->
                    [ monthFixed
                    , DateFormat.text "/"
                    , dayOfMonthFixed
                    , DateFormat.text "/"
                    , yearNumber
                    , DateFormat.text " "
                    , hourNumber
                    , DateFormat.text ":"
                    , minuteFixed
                    , DateFormat.text " "
                    , amPmUppercase
                    ]
    in
    (*) 1000
        >> Time.millisToPosix
        >> formatWithLanguage timeLang format zone


percentage : Model -> Float -> String
percentage model fl =
    fl
        * 100
        |> float (setDecimals model (FormatNumber.Locales.Max 2))


setDecimals : Model -> FormatNumber.Locales.Decimals -> Model
setDecimals model dec =
    { model | numberFormat = model.numberFormat |> s_decimals dec }
