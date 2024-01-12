module Config.Graph exposing (..)

import Api.Data
import Color exposing (Color)
import Dict exposing (Dict)


entityWidth : Float
entityWidth =
    190


layerMargin : Float
layerMargin =
    300


addressWidth : Float
addressWidth =
    entityWidth - 2 * padding - 2 * expandHandleWidth


addressHeight : Float
addressHeight =
    50


expandHandleWidth : Float
expandHandleWidth =
    15


entityTotalWidth : Float
entityTotalWidth =
    2 * expandHandleWidth + entityWidth


entityPaddingTop : Float
entityPaddingTop =
    10


padding : Float
padding =
    10


labelHeight : Float
labelHeight =
    18


addressesCountHeight : Float
addressesCountHeight =
    16


maxExpandableNeighbors : Int
maxExpandableNeighbors =
    25


maxExpandableAddresses : Int
maxExpandableAddresses =
    20


txMaxWidth : Float
txMaxWidth =
    7


arrowHeight : Float
arrowHeight =
    txMaxWidth


arrowWidth : Float
arrowWidth =
    arrowHeight


linkLabelHeight : Float
linkLabelHeight =
    12


entityMinHeight : Float
entityMinHeight =
    (entityPaddingTop + padding)
        + labelHeight
        + padding
        / 2
        + addressesCountHeight
        + padding


entityOneAddressHeight : Float
entityOneAddressHeight =
    entityMinHeight + addressHeight


entityToAddressesPaddingTop : Float
entityToAddressesPaddingTop =
    entityPaddingTop
        + padding
        + padding
        / 2
        + labelHeight


entityToAddressesPaddingLeft : Float
entityToAddressesPaddingLeft =
    expandHandleWidth
        + padding


minGapBetweenLayers : Float
minGapBetweenLayers =
    arrowHeight * 2


type AddressLabelType
    = ID
    | Balance
    | TotalReceived
    | Tag


type TxLabelType
    = NoTxs
    | Value


addressLabelToString : AddressLabelType -> String
addressLabelToString c =
    case c of
        ID ->
            "id"

        Balance ->
            "balance"

        TotalReceived ->
            "total received"

        Tag ->
            "tag"


stringToAddressLabel : String -> Maybe AddressLabelType
stringToAddressLabel s =
    case s of
        "id" ->
            Just ID

        "balance" ->
            Just Balance

        "tag" ->
            Just Tag

        "total received" ->
            Just TotalReceived

        _ ->
            Nothing


type alias Config =
    { addressLabelType : AddressLabelType
    , txLabelType : TxLabelType
    , maxLettersPerLabelRow : Int
    , colors : Dict String Color
    , entityConcepts : List Api.Data.Concept
    , abuseConcepts : List Api.Data.Concept
    , highlighter : Bool
    , showEntityShadowLinks : Bool
    , showAddressShadowLinks : Bool
    , showDatesInUserLocale : Bool
    , showZeroTransactions : Bool
    }


init : Maybe AddressLabelType -> Maybe TxLabelType -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Config
init addressLabelType txLabelType showEntityShadowLinks showAddressShadowLinks showDatesInUserLocale showZeroTransactions =
    { addressLabelType = addressLabelType |> Maybe.withDefault Tag
    , txLabelType = txLabelType |> Maybe.withDefault Value
    , maxLettersPerLabelRow = 18
    , colors = Dict.empty
    , entityConcepts = []
    , abuseConcepts = []
    , highlighter = False
    , showEntityShadowLinks = showEntityShadowLinks |> Maybe.withDefault True
    , showAddressShadowLinks = showAddressShadowLinks |> Maybe.withDefault False
    , showDatesInUserLocale = showDatesInUserLocale |> Maybe.withDefault True
    , showZeroTransactions = showZeroTransactions |> Maybe.withDefault True
    }
