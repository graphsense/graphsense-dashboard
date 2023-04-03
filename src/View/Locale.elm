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
    , tokenCurrencies
    , tokenCurrency
    )

import Api.Data
import Css exposing (num, opacity)
import Css.Transitions as T exposing (transition)
import DateFormat exposing (..)
import DateFormat.Relative
import Dict exposing (Dict)
import Ease
import Html.Styled exposing (Html, span, text)
import Html.Styled.Attributes exposing (css)
import List.Extra exposing (find)
import Locale.Durations
import Model.Currency exposing (..)
import Model.Locale exposing (..)
import RecordSetter exposing (..)
import String.Interpolate
import Time
import Tuple exposing (..)


fixpointFactor : Maybe Api.Data.TokenConfigs -> Dict String ( Float, String )
fixpointFactor configs =
    [ ( "eth", ( 1.0e18, "wei" ) )
    , ( "btc", ( 1.0e8, "s" ) )
    , ( "bch", ( 1.0e8, "s" ) )
    , ( "ltc", ( 1.0e8, "s" ) )
    , ( "zec", ( 1.0e8, "s" ) )
    ]
        ++ (configs
                |> Maybe.map
                    (.tokenConfigs
                        >> List.map
                            (\{ decimals, ticker } ->
                                ( ticker, ( 10 ^ toFloat decimals, "wei" ) )
                            )
                    )
                |> Maybe.withDefault []
           )
        |> Dict.fromList


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
    currencyWithOptions One


tokenCurrency : Model -> String -> Api.Data.Values -> String
tokenCurrency =
    currencyWithOptions Both


currencyWithoutCode : Model -> String -> Api.Data.Values -> String
currencyWithoutCode =
    currencyWithOptions Hidden


type CodeVisibility
    = Hidden
    | One
    | Both


currencyWithOptions : CodeVisibility -> Model -> String -> Api.Data.Values -> String
currencyWithOptions vis model coinCode values =
    case model.currency of
        Coin ->
            coin model (vis == Hidden) coinCode values.value

        Fiat code ->
            values.fiatValues
                |> List.filter (.code >> String.toLower >> (==) code)
                |> List.head
                |> Maybe.map (fiat model coinCode vis)
                |> Maybe.withDefault ""


fiat : Model -> String -> CodeVisibility -> Api.Data.Rate -> String
fiat model coinCode vis { code, value } =
    float model value
        ++ (case vis of
                Hidden ->
                    ""

                One ->
                    " " ++ String.toUpper code

                Both ->
                    " " ++ String.toUpper code ++ " (" ++ String.toUpper coinCode ++ ")"
           )


coin : Model -> Bool -> String -> Int -> String
coin model hideCode code v =
    fixpointFactor model.supportedTokens
        |> Dict.get code
        |> Maybe.map
            (mapFirst
                (\f ->
                    if v == 0 then
                        0

                    else
                        toFloat v / f
                )
            )
        |> Maybe.map
            (\( value, sc ) ->
                let
                    fmt =
                        if value == 0.0 then
                            "1,000"

                        else if abs value >= 1.0 then
                            "1,000.00"

                        else
                            let
                                n =
                                    find (\exp -> (abs value * (10 ^ toFloat exp)) >= 1) (List.range 0 10) |> Maybe.withDefault 2
                            in
                            "1,000." ++ String.repeat (n + 1) "0"
                in
                floatWithFormat model fmt value
                    ++ (if hideCode then
                            ""

                        else
                            " " ++ String.toUpper code
                       )
            )
        |> Maybe.withDefault ("unknown currency " ++ code)


durationToString : Model -> Int -> String
durationToString { unitToString } dur =
    Locale.Durations.durationToString
        { unitToString = unitToString
        , precision = 3
        , separator = " "
        }
        dur


tokenCurrencies : Model -> List String
tokenCurrencies model =
    model.supportedTokens
        |> Maybe.map (.tokenConfigs >> List.map .ticker)
        |> Maybe.withDefault []
