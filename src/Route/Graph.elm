module Route.Graph exposing
    ( ActorTable(..)
    , AddressTable(..)
    , AddresslinkTable(..)
    , BlockTable(..)
    , Config
    , EntityTable(..)
    , Route(..)
    , Thing(..)
    , TxTable(..)
    , actorRoute
    , addressPathRoute
    , addressRoute
    , addresslinkRoute
    , blockRoute
    , entityRoute
    , entitylinkRoute
    , getAddressTable
    , getAddresslinkTable
    , getEntityTable
    , labelRoute
    , parser
    , pluginRoute
    , resultLineToRoute
    , rootRoute
    , toUrl
    , txRoute
    )

import List.Extra
import Model.Search as Search exposing (ResultLine)
import Plugin.Model
import Plugin.Route as Plugin
import Url.Builder as B exposing (..)
import Util.Url.Parser as P exposing (..)
import Util.Url.Parser.Query as Q


type alias Config =
    { currencies : List String
    }


type Route
    = Currency String Thing
    | Label String
    | Actor String (Maybe ActorTable)
    | Root
    | Plugin ( Plugin.Model.PluginType, String )


type Thing
    = Address String (Maybe AddressTable) (Maybe Int)
    | Entity Int (Maybe EntityTable) (Maybe Int)
    | Block Int (Maybe BlockTable)
    | Tx String (Maybe TxTable) (Maybe Int)
    | Addresslink String Int String Int (Maybe AddresslinkTable)
    | Entitylink Int Int Int Int (Maybe AddresslinkTable)
    | AddressPath ( String, List String )


addressSegment : String
addressSegment =
    "address"


addresspathSegment : String
addresspathSegment =
    "addresspath"


entitySegment : String
entitySegment =
    "entity"


blockSegment : String
blockSegment =
    "block"


txSegment : String
txSegment =
    "tx"


addresslinkSegment : String
addresslinkSegment =
    "addresslink"


entitylinkSegment : String
entitylinkSegment =
    "entitylink"


labelSegment : String
labelSegment =
    "label"


actorSegment : String
actorSegment =
    "actor"


tableQuery : String
tableQuery =
    "table"


tokenTxIdQuery : String
tokenTxIdQuery =
    "token_tx_id"


addressPathSeparator : String
addressPathSeparator =
    ","


type AddressTable
    = AddressTagsTable
    | AddressTxsTable
    | AddressIncomingNeighborsTable
    | AddressOutgoingNeighborsTable
    | AddressTotalReceivedAllAssetsTable
    | AddressFinalBalanceAllAssetsTable


type EntityTable
    = EntityTagsTable
    | EntityTxsTable
    | EntityAddressesTable
    | EntityIncomingNeighborsTable
    | EntityOutgoingNeighborsTable
    | EntityTotalReceivedAllAssetsTable
    | EntityFinalBalanceAllAssetsTable


type ActorTable
    = ActorTagsTable
    | ActorOtherLinksTable


type TxTable
    = TxInputsTable
    | TxOutputsTable
    | TokenTxsTable


type BlockTable
    = BlockTxsTable


type AddresslinkTable
    = AddresslinkTxsTable
    | AddresslinkAllAssetsTable


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

        AddressTotalReceivedAllAssetsTable ->
            "total_received"

        AddressFinalBalanceAllAssetsTable ->
            "final_balance"


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

        "total_received" ->
            Just AddressTotalReceivedAllAssetsTable

        "final_balance" ->
            Just AddressFinalBalanceAllAssetsTable

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
            "incoming"

        EntityOutgoingNeighborsTable ->
            "outgoing"

        EntityTotalReceivedAllAssetsTable ->
            "total_received"

        EntityFinalBalanceAllAssetsTable ->
            "final_balance"


stringToEntityTable : String -> Maybe EntityTable
stringToEntityTable t =
    case t of
        "tags" ->
            Just EntityTagsTable

        "transactions" ->
            Just EntityTxsTable

        "addresses" ->
            Just EntityAddressesTable

        "incoming" ->
            Just EntityIncomingNeighborsTable

        "outgoing" ->
            Just EntityOutgoingNeighborsTable

        "total_received" ->
            Just EntityTotalReceivedAllAssetsTable

        "final_balance" ->
            Just EntityFinalBalanceAllAssetsTable

        _ ->
            Nothing


txTableToString : TxTable -> String
txTableToString t =
    case t of
        TxInputsTable ->
            "inputs"

        TxOutputsTable ->
            "outputs"

        TokenTxsTable ->
            "token_txs"


stringToActorTagsTable : String -> Maybe ActorTable
stringToActorTagsTable t =
    case t of
        "tags" ->
            Just ActorTagsTable

        "links" ->
            Just ActorOtherLinksTable

        _ ->
            Nothing


actorTableToString : ActorTable -> String
actorTableToString t =
    case t of
        ActorTagsTable ->
            "tags"

        ActorOtherLinksTable ->
            "links"


stringToTxTable : String -> Maybe TxTable
stringToTxTable t =
    case t of
        "inputs" ->
            Just TxInputsTable

        "outputs" ->
            Just TxOutputsTable

        "token_txs" ->
            Just TokenTxsTable

        _ ->
            Nothing


blockTableToString : BlockTable -> String
blockTableToString t =
    case t of
        BlockTxsTable ->
            "transactions"


stringToBlockTable : String -> Maybe BlockTable
stringToBlockTable t =
    case t of
        "transactions" ->
            Just BlockTxsTable

        _ ->
            Nothing


addresslinkTableToString : AddresslinkTable -> String
addresslinkTableToString t =
    case t of
        AddresslinkTxsTable ->
            "transactions"

        AddresslinkAllAssetsTable ->
            "allassets"


stringToAddresslinkTable : String -> Maybe AddresslinkTable
stringToAddresslinkTable t =
    case t of
        "transactions" ->
            Just AddresslinkTxsTable

        "allassets" ->
            Just AddresslinkAllAssetsTable

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

        Currency curr (Block block table) ->
            let
                query =
                    table
                        |> Maybe.map (blockTableToString >> B.string tableQuery >> List.singleton)
                        |> Maybe.withDefault []
            in
            absolute [ curr, blockSegment, String.fromInt block ] query

        Currency curr (Tx tx table tokenTxId) ->
            let
                query =
                    (table
                        |> Maybe.map (txTableToString >> B.string tableQuery >> List.singleton)
                        |> Maybe.withDefault []
                    )
                        ++ (tokenTxId
                                |> Maybe.map (String.fromInt >> B.string tokenTxIdQuery >> List.singleton)
                                |> Maybe.withDefault []
                           )
            in
            absolute [ curr, txSegment, tx ] query

        Currency curr (Addresslink src srcLayer dst dstLayer table) ->
            let
                query =
                    table
                        |> Maybe.map (addresslinkTableToString >> B.string tableQuery >> List.singleton)
                        |> Maybe.withDefault []
            in
            absolute
                [ curr
                , addresslinkSegment
                , src
                , String.fromInt srcLayer
                , dst
                , String.fromInt dstLayer
                ]
                query

        Currency curr (Entitylink src srcLayer dst dstLayer table) ->
            let
                query =
                    table
                        |> Maybe.map (addresslinkTableToString >> B.string tableQuery >> List.singleton)
                        |> Maybe.withDefault []
            in
            absolute
                [ curr
                , entitylinkSegment
                , String.fromInt src
                , String.fromInt srcLayer
                , String.fromInt dst
                , String.fromInt dstLayer
                ]
                query

        Currency curr (AddressPath ( address, addresses )) ->
            absolute
                [ curr
                , addresspathSegment
                , String.join addressPathSeparator <| address :: addresses
                ]
                []

        Label l ->
            absolute [ labelSegment, l ] []

        Actor actorId table ->
            let
                query =
                    table
                        |> Maybe.map (actorTableToString >> B.string tableQuery >> List.singleton)
                        |> Maybe.withDefault []
            in
            absolute [ actorSegment, actorId ] query

        Plugin ( ns, p ) ->
            "/" ++ Plugin.Model.pluginTypeToNamespace ns ++ "/" ++ p


rootRoute : Route
rootRoute =
    Root


addressRoute : { currency : String, address : String, layer : Maybe Int, table : Maybe AddressTable } -> Route
addressRoute { currency, address, layer, table } =
    Address address table layer
        |> Currency (String.toLower currency)


addresslinkRoute : { currency : String, src : String, srcLayer : Int, dst : String, dstLayer : Int, table : Maybe AddresslinkTable } -> Route
addresslinkRoute { currency, src, srcLayer, dst, dstLayer, table } =
    Addresslink src srcLayer dst dstLayer table
        |> Currency (String.toLower currency)


entitylinkRoute : { currency : String, src : Int, srcLayer : Int, dst : Int, dstLayer : Int, table : Maybe AddresslinkTable } -> Route
entitylinkRoute { currency, src, srcLayer, dst, dstLayer, table } =
    Entitylink src srcLayer dst dstLayer table
        |> Currency (String.toLower currency)


txRoute : { currency : String, txHash : String, table : Maybe TxTable, tokenTxId : Maybe Int } -> Route
txRoute { currency, txHash, table, tokenTxId } =
    Tx txHash table tokenTxId
        |> Currency (String.toLower currency)


blockRoute : { currency : String, block : Int, table : Maybe BlockTable } -> Route
blockRoute { currency, block, table } =
    Block block table
        |> Currency (String.toLower currency)


labelRoute : String -> Route
labelRoute =
    Label


actorRoute : String -> Maybe ActorTable -> Route
actorRoute id table =
    Actor id table


entityRoute : { currency : String, entity : Int, layer : Maybe Int, table : Maybe EntityTable } -> Route
entityRoute { currency, entity, layer, table } =
    Entity entity table layer
        |> Currency (String.toLower currency)


addressPathRoute : String -> ( String, List String ) -> Route
addressPathRoute currency path =
    AddressPath path
        |> Currency currency


pluginRoute : ( String, String ) -> Route
pluginRoute ( ns, url ) =
    ns
        |> Plugin.Model.namespaceToPluginType
        |> Maybe.map
            (\type_ ->
                ( type_
                , if String.startsWith "/" url then
                    String.dropLeft 1 url

                  else
                    url
                )
                    |> Plugin
            )
        |> Maybe.withDefault Root


parser : Config -> Parser (Route -> a) a
parser c =
    oneOf
        [ map Currency (parseCurrency c |> P.slash thing)
        , map Label (P.s labelSegment |> P.slash P.string)
        , map Actor (P.s actorSegment |> P.slash P.string |> P.questionMark (Q.string tableQuery |> Q.map (Maybe.andThen stringToActorTagsTable)))
        , map Plugin (P.remainder Plugin.parseUrl)
        , map Root P.top
        ]


parseCurrency : Config -> Parser (String -> a) a
parseCurrency c =
    P.custom "CURRENCY" <|
        \segment ->
            List.Extra.find ((==) segment) c.currencies


parseAddressPath : Parser (( String, List String ) -> a) a
parseAddressPath =
    P.custom "ADDRESS_PATH" <|
        \segment ->
            String.split addressPathSeparator segment
                |> List.Extra.uncons


thing : Parser (Thing -> a) a
thing =
    oneOf
        [ s addressSegment
            |> P.slash P.string
            |> P.questionMark (Q.string tableQuery |> Q.map (Maybe.andThen stringToAddressTable))
            |> P.slash (P.fragment (Maybe.andThen String.toInt))
            |> map Address
        , s addresspathSegment
            |> P.slash parseAddressPath
            |> map AddressPath
        , s entitySegment
            |> P.slash P.int
            |> P.questionMark (Q.string tableQuery |> Q.map (Maybe.andThen stringToEntityTable))
            |> P.slash (P.fragment (Maybe.andThen String.toInt))
            |> map Entity
        , s txSegment
            |> P.slash P.string
            |> P.questionMark (Q.string tableQuery |> Q.map (Maybe.andThen stringToTxTable))
            |> P.questionMark (Q.int tokenTxIdQuery)
            |> map Tx
        , s blockSegment
            |> P.slash P.int
            |> P.questionMark (Q.string tableQuery |> Q.map (Maybe.andThen stringToBlockTable))
            |> map Block
        , s addresslinkSegment
            |> P.slash P.string
            |> P.slash P.int
            |> P.slash P.string
            |> P.slash P.int
            |> P.questionMark (Q.string tableQuery |> Q.map (Maybe.andThen stringToAddresslinkTable))
            |> map Addresslink
        , s entitylinkSegment
            |> P.slash P.int
            |> P.slash P.int
            |> P.slash P.int
            |> P.slash P.int
            |> P.questionMark (Q.string tableQuery |> Q.map (Maybe.andThen stringToAddresslinkTable))
            |> map Entitylink
        ]


getAddressTable : Route -> Maybe AddressTable
getAddressTable route =
    case route of
        Currency _ (Address _ table _) ->
            table

        _ ->
            Nothing


getEntityTable : Route -> Maybe EntityTable
getEntityTable route =
    case route of
        Currency _ (Entity _ table _) ->
            table

        _ ->
            Nothing


getAddresslinkTable : Route -> Maybe AddresslinkTable
getAddresslinkTable route =
    case route of
        Currency _ (Entitylink _ _ _ _ table) ->
            table

        Currency _ (Addresslink _ _ _ _ table) ->
            table

        _ ->
            Nothing


resultLineToRoute : ResultLine -> Route
resultLineToRoute resultLine =
    case resultLine of
        Search.Address currency address ->
            addressRoute
                { currency = currency
                , address = address
                , table = Nothing
                , layer = Nothing
                }

        Search.Tx currency tx ->
            txRoute
                { currency = currency
                , txHash = tx
                , table = Nothing
                , tokenTxId = Nothing
                }

        Search.Block currency block ->
            blockRoute
                { currency = currency
                , block = block
                , table = Nothing
                }

        Search.Label label ->
            labelRoute label

        Search.Actor ( id, _ ) ->
            actorRoute id Nothing

        Search.Custom _ ->
            Root
