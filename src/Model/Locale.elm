module Model.Locale exposing (Model, State(..), ValueDetail(..), getFiatValue, getTokenTickers, locales)

import Api.Data
import DateFormat
import Dict exposing (Dict)
import Locale.Durations
import Time


locales : List ( String, String )
locales =
    [ ( "de", "Deutsch" )
    , ( "en", "English" )
    , ( "it", "Italiano" )
    , ( "es", "Español" )
    , ( "ro", "Română" )
    ]


type State
    = Empty
    | Transition (Dict String String) (Dict String String) Float
    | Settled (Dict String String)


type ValueDetail
    = Exact
    | Magnitude


type alias Model =
    { mapping : State
    , numberFormat : String -> Float -> String
    , valueDetail : ValueDetail
    , locale : String
    , zone : Time.Zone
    , timeLang : DateFormat.Language
    , unitToString : Int -> Locale.Durations.Unit -> String
    , supportedTokens : Dict String Api.Data.TokenConfigs
    }


getFiatValue : String -> Api.Data.Values -> Maybe Float
getFiatValue code values =
    values.fiatValues
        |> List.filter (.code >> String.toLower >> (==) code)
        |> List.head
        |> Maybe.map .value


getTokenTickers : Model -> String -> List String
getTokenTickers m net =
    Dict.get net m.supportedTokens
        |> Maybe.map (.tokenConfigs >> List.map (.ticker >> String.toUpper))
        |> Maybe.withDefault []
