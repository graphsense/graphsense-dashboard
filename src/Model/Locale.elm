module Model.Locale exposing (Flags, Model, State(..), ValueDetail(..), getFiatValue, getTokenTickers, isEmpty, locales)

import Api.Data
import DateFormat.Language
import DateFormat.Relative
import Dict exposing (Dict)
import Locale.Durations
import Model.Currency exposing (..)
import Time


locales : List ( String, String )
locales =
    [ ( "de", "German" ), ( "en", "English" ), ( "it", "Italiano" ) ]


type State
    = Empty
    | Transition (Dict String String) (Dict String String) Float
    | Settled (Dict String String)


type ValueDetail
    = Exact
    | Magnitude


{-|

    locale : the two digit locale id
    hint : a number formatted via JS toLocaleString to derive number formatting
           rules from (see https://package.elm-lang.org/packages/cuducos/elm-format-number/latest/FormatNumber-Locales)

-}
type alias Flags =
    { locale : String
    }


type alias Model =
    { mapping : State
    , numberFormat : String -> Float -> String
    , valueDetail : ValueDetail
    , locale : String
    , zone : Time.Zone
    , timeLang : DateFormat.Language.Language
    , currency : Currency
    , relativeTimeOptions : DateFormat.Relative.RelativeTimeOptions
    , unitToString : Int -> Locale.Durations.Unit -> String
    , supportedTokens : Dict String Api.Data.TokenConfigs
    }


getFiatValue : String -> Api.Data.Values -> Maybe Float
getFiatValue code values =
    values.fiatValues
        |> List.filter (.code >> String.toLower >> (==) code)
        |> List.head
        |> Maybe.map .value


isEmpty : Model -> Bool
isEmpty { mapping } =
    case mapping of
        Settled _ ->
            False

        _ ->
            True


getTokenTickers : Model -> String -> List String
getTokenTickers m net =
    Dict.get net m.supportedTokens
        |> Maybe.map (.tokenConfigs >> List.map (.ticker >> String.toUpper))
        |> Maybe.withDefault []
