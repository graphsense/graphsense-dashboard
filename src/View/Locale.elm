module View.Locale exposing
    ( coin
    , coinWithoutCode
    , currency
    , currencyAsFloat
    , currencyWithoutCode
    , currencyWithoutCode2
    , date
    , durationPosix
    , durationToString
    , durationToStringWithPrecision
    , fiat
    , fiatWithoutCode
    , httpErrorToString
    , int
    , intWithFormat
    , intWithoutValueDetailFormatting
    , interpolated
    , isFirstSecondOfTheDay
    , isLastSecondOfTheDay
    , percentage
    , posixDate
    , posixDateTimeUniform
    , posixDateUniform
    , posixToTimestampSeconds
    , relativeTime
    , string
    , text
    , time
    , timestamp
    , timestampDateTimeUniform
    , timestampDateUniform
    , timestampTimeUniform
    , timestampWithFormat
    , titleCase
    , tokenCurrencies
    , valuesToFloat
    )

import Api.Data
import Basics.Extra exposing (flip)
import Css exposing (num, opacity)
import Css.Transitions as T exposing (transition)
import DateFormat exposing (..)
import DateFormat.Relative
import Dict exposing (Dict)
import Ease
import Html.Styled exposing (Html, span)
import Html.Styled.Attributes exposing (css)
import Http
import List.Extra exposing (find)
import Locale.Durations
import Model.Currency exposing (..)
import Model.Locale exposing (..)
import String.Extra
import String.Interpolate
import Time exposing (Posix)
import Time.Extra exposing (toOffset)
import Tuple exposing (..)
import Util.Data exposing (timestampToPosix)


type CodeVisibility
    = Hidden
    | One


fixpointFactor : Maybe Api.Data.TokenConfigs -> Dict String ( Float, String )
fixpointFactor configs =
    [ ( "eth", ( 1.0e18, "wei" ) )
    , ( "trx", ( 1.0e6, "sun" ) )
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


formatWithValueDetail : Model -> String -> String
formatWithValueDetail model fmtStr =
    case model.valueDetail of
        Exact ->
            if model.currency == Coin then
                fmtStr ++ "[00000000000000000]"

            else
                fmtStr ++ "0"

        Magnitude ->
            if String.endsWith fmtStr "a" then
                fmtStr

            else
                fmtStr ++ "a"


float : Model -> Float -> String
float model value =
    model.numberFormat (formatWithValueDetail model "1,000.0") value


floatWithFormat : Model -> String -> Float -> String
floatWithFormat model fmtStr value =
    model.numberFormat (formatWithValueDetail model fmtStr) value


int : Model -> Int -> String
int model =
    toFloat
        >> floatWithFormat model "1,000"


intWithoutValueDetailFormatting : Model -> Int -> String
intWithoutValueDetailFormatting model =
    toFloat
        >> model.numberFormat "1,000"


intWithFormat : Model -> String -> Int -> String
intWithFormat model format =
    toFloat >> floatWithFormat model format


posixToTimestampSeconds : Posix -> Int
posixToTimestampSeconds =
    Time.posixToMillis >> flip (//) 1000


isFirstSecondOfTheDay : Model -> Posix -> Bool
isFirstSecondOfTheDay m d =
    let
        hour =
            Time.toHour m.zone d

        min =
            Time.toMinute m.zone d

        s =
            Time.toSecond m.zone d
    in
    hour == 0 && min == 0 && s == 0


isLastSecondOfTheDay : Model -> Posix -> Bool
isLastSecondOfTheDay m d =
    let
        hour =
            Time.toHour m.zone d

        min =
            Time.toMinute m.zone d

        s =
            Time.toSecond m.zone d
    in
    hour == 23 && min == 59 && s == 59


posixDate : Model -> Posix -> String
posixDate m d =
    date m (posixToTimestampSeconds d)


posixDateUniform : Model -> Posix -> String
posixDateUniform m d =
    timestampDateUniform m (posixToTimestampSeconds d)


posixDateTimeUniform : Model -> Bool -> Posix -> String
posixDateTimeUniform m showTimeZoneOffset d =
    timestampDateTimeUniform m showTimeZoneOffset (posixToTimestampSeconds d)


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


timestampDateUniform : Model -> Int -> String
timestampDateUniform model =
    let
        format =
            [ monthNameAbbreviated
            , DateFormat.text " "

            --, dayOfMonthSuffix
            , dayOfMonthFixed
            , DateFormat.text ", "
            , yearNumber
            ]
    in
    timestampWithFormat format model



-- { model | zone = Time.utc }


timestampTimeUniform : Model -> Bool -> Int -> String
timestampTimeUniform model showTimeZoneOffset x =
    let
        timezoneOffset =
            if showTimeZoneOffset then
                "+" ++ (toOffset model.zone (timestampToPosix x) |> flip (//) 60 |> String.fromInt)

            else
                ""

        format =
            [ hourMilitaryFixed
            , DateFormat.text ":"
            , minuteFixed
            , DateFormat.text ":"
            , secondFixed
            , DateFormat.text " "

            --, amPmUppercase
            ]
    in
    timestampWithFormat format model x ++ timezoneOffset



--{ model | zone = Time.utc }


timestampDateTimeUniform : Model -> Bool -> Int -> String
timestampDateTimeUniform model showTimeZoneOffset x =
    timestampDateUniform model x ++ " " ++ timestampTimeUniform model showTimeZoneOffset x


date : Model -> Int -> String
date model =
    let
        format =
            case model.locale of
                "de" ->
                    [ dayOfMonthFixed
                    , DateFormat.text "."
                    , monthFixed
                    , DateFormat.text "."
                    , yearNumberLastTwo
                    ]

                _ ->
                    [ monthFixed
                    , DateFormat.text "/"
                    , dayOfMonthFixed
                    , DateFormat.text "/"
                    , yearNumberLastTwo
                    ]
    in
    timestampWithFormat format model


time : Model -> Int -> String
time model =
    let
        format =
            case model.locale of
                "de" ->
                    [ hourMilitaryFixed
                    , DateFormat.text ":"
                    , minuteFixed
                    , DateFormat.text ":"
                    , secondFixed
                    ]

                _ ->
                    [ hourFixed
                    , DateFormat.text ":"
                    , minuteFixed
                    , DateFormat.text ":"
                    , secondFixed
                    , DateFormat.text " "
                    , amPmUppercase
                    ]
    in
    timestampWithFormat format model


timestampWithFormat : List Token -> Model -> Int -> String
timestampWithFormat format { timeLang, zone } =
    timestampToPosix
        >> formatWithLanguage timeLang format zone


relativeTime : Model -> Time.Posix -> Int -> String
relativeTime { relativeTimeOptions } from to =
    DateFormat.Relative.relativeTimeWithOptions relativeTimeOptions
        from
        (timestampToPosix to)


percentage : Model -> Float -> String
percentage model =
    floatWithFormat model "0[.]00%"


bestAssetAsInt : Model -> List ( AssetIdentifier, Api.Data.Values ) -> Maybe ( AssetIdentifier, Int )
bestAssetAsInt _ =
    let
        fiatValue v =
            v.fiatValues
                |> List.head
                |> Maybe.map .value
                |> Maybe.withDefault (toFloat v.value)
    in
    List.sortBy (second >> fiatValue)
        >> List.reverse
        >> List.head
        >> Maybe.map (mapSecond .value)


sumFiats : String -> List ( AssetIdentifier, Api.Data.Values ) -> Float
sumFiats fiatCode =
    List.filterMap (second >> getFiatValue fiatCode)
        >> List.sum


currencyAsFloat : Model -> List ( AssetIdentifier, Api.Data.Values ) -> Float
currencyAsFloat model values =
    case model.currency of
        Coin ->
            bestAssetAsInt model values
                |> Maybe.map (second >> toFloat)
                |> Maybe.withDefault 0

        Fiat code ->
            sumFiats code values


currencyWithOptions : CodeVisibility -> Model -> List ( AssetIdentifier, Api.Data.Values ) -> String
currencyWithOptions vis model values =
    case model.currency of
        Coin ->
            if List.all (second >> .value >> (==) 0) values then
                "0"

            else
                bestAssetAsInt model values
                    |> Maybe.map
                        (\( asset, value ) ->
                            coinWithOptions vis model asset value
                                ++ (if List.length values == 1 then
                                        ""

                                    else
                                        " +"
                                            ++ (List.length values - 1 |> String.fromInt)
                                   )
                        )
                    |> Maybe.withDefault "0"

        Fiat code ->
            sumFiats code values
                |> fiat model code


currencyWithOptions2 : CodeVisibility -> Model -> List ( AssetIdentifier, Api.Data.Values ) -> String
currencyWithOptions2 vis model values =
    case model.currency of
        Coin ->
            if List.all (second >> .value >> (==) 0) values then
                "0"

            else
                bestAssetAsInt model values
                    |> Maybe.map
                        (\( asset, value ) ->
                            coinWithOptions vis model asset value
                                ++ (if List.length values == 1 then
                                        ""

                                    else
                                        " +"
                                            ++ (List.length values - 1 |> String.fromInt)
                                   )
                        )
                    |> Maybe.withDefault "0"

        Fiat code ->
            sumFiats code values
                |> fiatWithOptions vis model code


currency : Model -> List ( AssetIdentifier, Api.Data.Values ) -> String
currency =
    currencyWithOptions One


currencyWithoutCode : Model -> List ( AssetIdentifier, Api.Data.Values ) -> String
currencyWithoutCode =
    currencyWithOptions Hidden


currencyWithoutCode2 : Model -> List ( AssetIdentifier, Api.Data.Values ) -> String
currencyWithoutCode2 =
    currencyWithOptions2 Hidden


fiat : Model -> String -> Float -> String
fiat =
    fiatWithOptions One


fiatWithoutCode : Model -> String -> Float -> String
fiatWithoutCode =
    fiatWithOptions Hidden


fiatWithOptions : CodeVisibility -> Model -> String -> Float -> String
fiatWithOptions vis model code value =
    float model value
        ++ (case vis of
                Hidden ->
                    ""

                One ->
                    " " ++ String.toUpper code
           )


coin : Model -> AssetIdentifier -> Int -> String
coin =
    coinWithOptions One


coinWithoutCode : Model -> AssetIdentifier -> Int -> String
coinWithoutCode =
    coinWithOptions Hidden


coinWithOptions : CodeVisibility -> Model -> AssetIdentifier -> Int -> String
coinWithOptions vis model asset v =
    normalizeCoinValue model asset v
        |> Maybe.map
            (\value ->
                let
                    fmt =
                        formatWithValueDetail model <|
                            if value == 0.0 then
                                "1,000"

                            else if abs value > 1.0 then
                                "1,000.00"

                            else
                                let
                                    n =
                                        List.range 0 14
                                            |> find
                                                (\exp -> (abs value * (10 ^ toFloat exp)) > 1)
                                            |> Maybe.withDefault 2
                                in
                                "1,000." ++ String.repeat (n + 1) "0"
                in
                floatWithFormat model fmt value
                    ++ (if vis == Hidden then
                            ""

                        else
                            " " ++ String.toUpper asset.asset
                       )
            )
        |> Maybe.withDefault ("unknown currency " ++ asset.asset)


normalizeCoinValue : Model -> AssetIdentifier -> Int -> Maybe Float
normalizeCoinValue model asset v =
    fixpointFactor (Dict.get asset.network model.supportedTokens)
        |> Dict.get (String.toLower asset.asset)
        |> Maybe.map first
        |> Maybe.map
            (\f ->
                if v == 0 then
                    0

                else
                    toFloat v / f
            )


valuesToFloat : Model -> AssetIdentifier -> Api.Data.Values -> Maybe Float
valuesToFloat model asset values =
    case model.currency of
        Coin ->
            values.value
                |> normalizeCoinValue model asset

        Fiat curr ->
            List.Extra.find (.code >> (==) curr) values.fiatValues
                |> Maybe.map .value


durationToString : Model -> Int -> String
durationToString m dur =
    durationToStringWithPrecision m 3 dur


durationPosix : Model -> Int -> Posix -> Posix -> String
durationPosix m prec start end =
    durationToStringWithPrecision m prec (Time.posixToMillis end - Time.posixToMillis start)


durationToStringWithPrecision : Model -> Int -> Int -> String
durationToStringWithPrecision { unitToString } prec dur =
    Locale.Durations.durationToString
        { unitToString = unitToString
        , precision = prec
        , separator = " "
        }
        dur


tokenCurrencies : String -> Model -> List String
tokenCurrencies network model =
    getTokenTickers model network


httpErrorToString : Model -> Http.Error -> String
httpErrorToString model error =
    case error of
        Http.BadUrl url ->
            string model "bad url" ++ " " ++ url

        Http.Timeout ->
            string model "timeout"

        Http.NetworkError ->
            string model "network error"

        Http.BadStatus 500 ->
            string model "server error"

        Http.BadStatus 429 ->
            string model "API rate limit exceeded. Please try again later."

        Http.BadStatus 404 ->
            string model "not found"

        Http.BadStatus 504 ->
            string model "timeout"

        Http.BadStatus s ->
            string model "bad status" ++ ": " ++ String.fromInt s

        Http.BadBody str ->
            string model str


titleCase : Model -> String -> String
titleCase model =
    if model.locale == "en" then
        String.Extra.toTitleCase

    else
        identity
