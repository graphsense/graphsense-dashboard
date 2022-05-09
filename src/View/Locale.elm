module View.Locale exposing (currency, float, floatWithFormat, int, intWithFormat, interpolated, percentage, string, text, timestamp)

import Api.Data
import Css exposing (num, opacity)
import Css.Transitions as T exposing (transition)
import DateFormat exposing (..)
import Dict
import Ease
import FormatNumber
import FormatNumber.Locales
import Html.Styled exposing (Html, span, text)
import Html.Styled.Attributes exposing (css)
import Model.Locale exposing (..)
import RecordSetter exposing (..)
import String.Interpolate
import Time
import Update.Locale exposing (duration)


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


interpolated : Model -> String -> List String -> String
interpolated model key =
    String.Interpolate.interpolate (string model key)


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
    numberFormat "1,000.0" x


floatWithFormat : Model -> String -> Float -> String
floatWithFormat { numberFormat } =
    numberFormat


int : Model -> Int -> String
int model =
    toFloat
        >> floatWithFormat model "1,000"


intWithFormat : Model -> String -> Int -> String
intWithFormat model format =
    toFloat >> floatWithFormat model format


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
percentage model =
    floatWithFormat model "100[.00]%"


currency : Model -> String -> Api.Data.Values -> String
currency model coinCode values =
    case model.currency of
        Coin ->
            coin model coinCode values.value

        Fiat code ->
            values.fiatValues
                |> List.filter (.code >> String.toLower >> (==) code)
                |> List.head
                |> Maybe.map (fiat model)
                |> Maybe.withDefault ""


fiat : Model -> Api.Data.Rate -> String
fiat model { code, value } =
    float model value ++ " " ++ String.toUpper code


coin : Model -> String -> Int -> String
coin model code v =
    let
        ( value, sc ) =
            if code == "eth" then
                ( toFloat v / 1.0e18, "wei" )

            else
                ( toFloat v / 1.0e8, "s" )
    in
    if abs value < 0.0001 then
        int model v ++ " " ++ sc

    else
        float model value ++ " " ++ String.toUpper code
