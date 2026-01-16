module View.Locale exposing
    ( coin
    , coinWithoutCode
    , currency
    , currencyAsFloat
    , currencyWithoutCode
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
    , makeTimestampFilename
    , percentage
    , string
    , text
    , time
    , timestamp
    , timestampDateTimeUniform
    , timestampDateUniform
    , timestampNormal
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
import DateFormat
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
            fmtStr

        Magnitude ->
            fmtStr
                ++ (if model.locale == "de" then
                        " a"

                    else
                        "a"
                   )


int : Model -> Int -> String
int model =
    intWithFormat model (formatWithValueDetail model "1,000")


intWithoutValueDetailFormatting : Model -> Int -> String
intWithoutValueDetailFormatting model =
    toFloat
        >> model.numberFormat "1,000"


intWithFormat : Model -> String -> Int -> String
intWithFormat model format =
    toFloat >> model.numberFormat format


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


timestamp : Model -> Posix -> String
timestamp model =
    let
        format =
            string model "date-time-format"
    in
    timestampWithFormat format model


timestampDateUniform : Model -> Posix -> String
timestampDateUniform model =
    let
        format =
            "MMM dd, yyyy"
    in
    timestampWithFormat format model



-- { model | zone = Time.utc }


timestampTimeUniform : Model -> Bool -> Posix -> String
timestampTimeUniform model showTimeZoneOffset x =
    let
        timezoneOffset =
            if showTimeZoneOffset then
                "+" ++ (toOffset model.zone x |> flip (//) 60 |> String.fromInt)

            else
                ""

        format =
            "HH:mm:ss "
    in
    timestampWithFormat format model x ++ timezoneOffset



--{ model | zone = Time.utc }


timestampDateTimeUniform : Model -> Bool -> Posix -> String
timestampDateTimeUniform model showTimeZoneOffset x =
    timestampDateUniform model x ++ " " ++ timestampTimeUniform model showTimeZoneOffset x


date : Model -> Posix -> String
date model =
    let
        format =
            string model "date-format"
    in
    timestampWithFormat format model


time : Model -> Posix -> String
time model =
    let
        format =
            string model "time-format"
    in
    timestampWithFormat format model


timestampWithFormat : String -> Model -> Posix -> String
timestampWithFormat format { timeLang, zone } =
    DateFormat.formatI18n timeLang format zone


percentage : Model -> Float -> String
percentage model =
    model.numberFormat "0[.]00%"


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


currencyAsFloat : Currency -> Model -> List ( AssetIdentifier, Api.Data.Values ) -> Float
currencyAsFloat c model values =
    currencyWithOptions { showCode = False, currency = c } model values
        |> String.toFloat
        |> Maybe.withDefault 0


type alias CurrencyOptions =
    { showCode : Bool
    , currency : Currency
    }


currencyWithOptions : CurrencyOptions -> Model -> List ( AssetIdentifier, Api.Data.Values ) -> String
currencyWithOptions options model values =
    case options.currency of
        Coin ->
            if List.length values > 1 && allZero values then
                "0"

            else
                bestAssetAsInt model values
                    |> Maybe.map
                        (\( asset, value ) ->
                            coinWithOptions options.showCode model asset value
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


currency : Currency -> Model -> List ( AssetIdentifier, Api.Data.Values ) -> String
currency c =
    currencyWithOptions { showCode = True, currency = c }


currencyWithoutCode : Currency -> Model -> List ( AssetIdentifier, Api.Data.Values ) -> String
currencyWithoutCode c =
    currencyWithOptions { showCode = False, currency = c }


fiat : Model -> String -> Float -> String
fiat =
    fiatWithOptions True


fiatWithoutCode : Model -> String -> Float -> String
fiatWithoutCode =
    fiatWithOptions False


fiatWithOptions : Bool -> Model -> String -> Float -> String
fiatWithOptions showCode model code value =
    model.numberFormat (formatWithValueDetail model "1,000.00") value
        ++ (if not showCode then
                ""

            else
                " " ++ String.toUpper code
           )


coin : Model -> AssetIdentifier -> Int -> String
coin =
    coinWithOptions True


coinWithoutCode : Model -> AssetIdentifier -> Int -> String
coinWithoutCode =
    coinWithOptions False


coinWithOptions : Bool -> Model -> AssetIdentifier -> Int -> String
coinWithOptions showCode model asset v =
    normalizeCoinValue model asset v
        |> Maybe.map
            (\value ->
                let
                    fmt =
                        (if value == 0.0 then
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
                        )
                            |> (\f ->
                                    if model.valueDetail == Exact then
                                        f ++ "[00000000000000000]"

                                    else
                                        f
                               )
                            |> formatWithValueDetail model
                in
                model.numberFormat fmt value
                    ++ (if not showCode then
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


valuesToFloat : Currency -> Model -> AssetIdentifier -> Api.Data.Values -> Maybe Float
valuesToFloat c model asset values =
    case c of
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
            string model "Api-rate-limit-exceeded"

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
            >> String.split " "
            >> List.indexedMap
                (\i word ->
                    if i > 0 && String.length word <= 2 then
                        String.toLower word

                    else
                        word
                )
            >> String.join " "

    else
        identity


makeTimestampFilename : Model -> Time.Posix -> String
makeTimestampFilename locale =
    timestampWithFormat "yyyy-MM-dd HH-mm-ss" locale


timestampNormal : Model -> Posix -> String
timestampNormal locale =
    timestampWithFormat "yyyy-MM-dd HH:mm:ss" locale
