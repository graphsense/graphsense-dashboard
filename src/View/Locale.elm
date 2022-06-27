module View.Locale exposing
    ( currency
    , currencyWithoutCode
    , durationToString
    , float
    , floatWithFormat
    , int
    , intWithFormat
    , interpolated
    , percentage
    , relativeTime
    , string
    , text
    , timestamp
    , timestampWithFormat
    )

import Api.Data
import Css exposing (num, opacity)
import Css.Transitions as T exposing (transition)
import DateFormat exposing (..)
import DateFormat.Relative
import Dict
import Ease
import FormatNumber
import FormatNumber.Locales
import Html.Styled exposing (Html, span, text)
import Html.Styled.Attributes exposing (css)
import Locale.Durations
import Model.Currency exposing (..)
import Model.Locale exposing (..)
import RecordSetter exposing (..)
import String.Interpolate
import Time
import Update.Locale exposing (duration)


string : Model -> String -> String
string model key =
    let
        lower =
            (String.left 1 key
                |> String.toLower
            )
                ++ String.dropLeft 1 key

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
timestamp model =
    let
        format =
            case model.locale of
                "de" ->
                    [ dayOfMonthNumber
                    , DateFormat.text ". "
                    , monthNameFull
                    , DateFormat.text " "
                    , yearNumber
                    , DateFormat.text " "
                    , hourMilitaryFixed
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
                    , hourFixed
                    , DateFormat.text ":"
                    , minuteFixed
                    , DateFormat.text " "
                    , amPmUppercase
                    ]
    in
    timestampWithFormat format model


timestampWithFormat : List Token -> Model -> Int -> String
timestampWithFormat format { locale, timeLang, zone } =
    (*) 1000
        >> Time.millisToPosix
        >> formatWithLanguage timeLang format zone


relativeTime : Model -> Time.Posix -> Int -> String
relativeTime { relativeTimeOptions } from to =
    DateFormat.Relative.relativeTimeWithOptions relativeTimeOptions
        from
        (Time.millisToPosix <| to * 1000)


percentage : Model -> Float -> String
percentage model =
    floatWithFormat model "0[.]00%"


currency : Model -> String -> Api.Data.Values -> String
currency =
    currencyWithOptions False


currencyWithoutCode : Model -> String -> Api.Data.Values -> String
currencyWithoutCode =
    currencyWithOptions True


currencyWithOptions : Bool -> Model -> String -> Api.Data.Values -> String
currencyWithOptions hideCode model coinCode values =
    case model.currency of
        Coin ->
            coin model hideCode coinCode values.value

        Fiat code ->
            values.fiatValues
                |> List.filter (.code >> String.toLower >> (==) code)
                |> List.head
                |> Maybe.map (fiat model hideCode)
                |> Maybe.withDefault ""


fiat : Model -> Bool -> Api.Data.Rate -> String
fiat model hideCode { code, value } =
    float model value
        ++ (if hideCode then
                ""

            else
                " " ++ String.toUpper code
           )


coin : Model -> Bool -> String -> Int -> String
coin model hideCode code v =
    let
        ( value, sc ) =
            if code == "eth" then
                ( toFloat v / 1.0e18, "wei" )

            else
                ( toFloat v / 1.0e8, "s" )
    in
    if abs value < 0.0001 then
        -- always show small currency
        int model v ++ sc

    else
        floatWithFormat model "1,000.0000" value
            ++ (if hideCode then
                    ""

                else
                    " " ++ String.toUpper code
               )


durationToString : Model -> Int -> String
durationToString { unitToString } dur =
    Locale.Durations.durationToString
        { unitToString = unitToString
        , precision = 3
        , separator = " "
        }
        dur
