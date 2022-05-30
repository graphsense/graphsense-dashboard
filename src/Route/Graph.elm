module Route.Graph exposing
    ( AddressTable(..)
    , Config
    , EntityTable(..)
    , Route(..)
    , Thing(..)
    , addressRoute
    , entityRoute
    , parse
    , parser
    , pluginRoute
    , rootRoute
    , toUrl
    )

import Json.Encode
import List.Extra
import Maybe.Extra
import Plugin exposing (Plugins)
import Url exposing (..)
import Url.Builder as B exposing (..)
import Util.Url.Parser as P exposing (..)
import Util.Url.Parser.Query as Q


type alias Config =
    { currencies : List String
    }


type Route
    = Currency String Thing
    | Label String
    | Root
    | Plugin ( String, String )


type Thing
    = Address String (Maybe AddressTable) (Maybe Int)
    | Entity Int (Maybe EntityTable) (Maybe Int)
    | Block Int
    | Tx String


addressSegment : String
addressSegment =
    "address"


entitySegment : String
entitySegment =
    "entity"


blockSegment : String
blockSegment =
    "block"


txSegment : String
txSegment =
    "tx"


labelSegment : String
labelSegment =
    "label"


tableQuery : String
tableQuery =
    "table"


type AddressTable
    = AddressTagsTable
    | AddressTxsTable
    | AddressIncomingNeighborsTable
    | AddressOutgoingNeighborsTable


type EntityTable
    = EntityTagsTable
    | EntityTxsTable
    | EntityAddressesTable
    | EntityIncomingNeighborsTable
    | EntityOutgoingNeighborsTable


addressTableToString : AddressTable -> String
addressTableToString t =
    case t of
        AddressTagsTable ->
            "tags"

        AddressTxsTable ->
            "transactions"

        AddressIncomingNeighborsTable ->
            "incoming"

        AddressOutgoingNeighborsTable ->
            "outgoing"


stringToAddressTable : String -> Maybe AddressTable
stringToAddressTable t =
    case t of
        "tags" ->
            Just AddressTagsTable

        "transactions" ->
            Just AddressTxsTable

        "incoming" ->
            Just AddressIncomingNeighborsTable

        "outgoing" ->
            Just AddressOutgoingNeighborsTable

        _ ->
            Nothing


entityTableToString : EntityTable -> String
entityTableToString t =
    case t of
        EntityTagsTable ->
            "tags"

        EntityTxsTable ->
            "transactions"

        EntityAddressesTable ->
            "addresses"

        EntityIncomingNeighborsTable ->
            "incoming-neighbors"

        EntityOutgoingNeighborsTable ->
            "outgoing-neighbors"


stringToEntityTable : String -> Maybe EntityTable
stringToEntityTable t =
    case t of
        "tags" ->
            Just EntityTagsTable

        "transactions" ->
            Just EntityTxsTable

        "addresses" ->
            Just EntityAddressesTable

        "incoming-neighbors" ->
            Just EntityIncomingNeighborsTable

        "outgoing-neighbors" ->
            Just EntityOutgoingNeighborsTable

        _ ->
            Nothing


toUrl : Route -> String
toUrl route =
    case route of
        Root ->
            absolute [] []

        Currency curr (Address address table layer) ->
            let
                query =
                    table
                        |> Maybe.map (addressTableToString >> B.string tableQuery >> List.singleton)
                        |> Maybe.withDefault []
            in
            Maybe.map String.fromInt layer
                |> B.custom Absolute [ curr, addressSegment, address ] query

        Currency curr (Entity entity table layer) ->
            let
                query =
                    table
                        |> Maybe.map (entityTableToString >> B.string tableQuery >> List.singleton)
                        |> Maybe.withDefault []
            in
            Maybe.map String.fromInt layer
                |> B.custom Absolute [ curr, entitySegment, String.fromInt entity ] query

        Currency curr (Block block) ->
            absolute [ curr, blockSegment, String.fromInt block ] []

        Currency curr (Tx tx) ->
            absolute [ curr, txSegment, tx ] []

        Label l ->
            absolute [ labelSegment, l ] []

        Plugin ( pid, p ) ->
            "/" ++ pid ++ p



--++ p


rootRoute : Route
rootRoute =
    Root


addressRoute : { currency : String, address : String, layer : Maybe Int, table : Maybe AddressTable } -> Route
addressRoute { currency, address, layer, table } =
    Address address table layer
        |> Currency currency


entityRoute : { currency : String, entity : Int, layer : Maybe Int, table : Maybe EntityTable } -> Route
entityRoute { currency, entity, layer, table } =
    Entity entity table layer
        |> Currency currency


pluginRoute : ( String, String ) -> Route
pluginRoute =
    Plugin


parse : Plugins -> Config -> Url -> Maybe Route
parse plugins c =
    P.parse (parser plugins c)


parser : Plugins -> Config -> Parser (Route -> a) a
parser plugins c =
    oneOf
        [ map Currency (parseCurrency c |> P.slash thing)
        , map Label (P.s labelSegment |> P.slash P.string)
        , map Plugin (P.remainder (Plugin.parseUrl plugins))
        , map Root P.top
        ]


parseCurrency : Config -> Parser (String -> a) a
parseCurrency c =
    P.custom "CURRENCY" <|
        \segment ->
            List.Extra.find ((==) segment) c.currencies


thing : Parser (Thing -> a) a
thing =
    oneOf
        [ s addressSegment
            |> P.slash P.string
            |> P.questionMark (Q.string tableQuery |> Q.map (Maybe.andThen stringToAddressTable))
            |> P.slash (P.fragment (Maybe.andThen String.toInt))
            |> map Address
        , s entitySegment
            |> P.slash P.int
            |> P.questionMark (Q.string tableQuery |> Q.map (Maybe.andThen stringToEntityTable))
            |> P.slash (P.fragment (Maybe.andThen String.toInt))
            |> map Entity

        {- , s blockSegment
               </> P.int
               |> map Block
           , s txSegment
               </> P.string
               |> map Tx
        -}
        ]
