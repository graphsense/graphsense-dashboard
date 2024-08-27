module Effect.Api exposing (..)

import Api
import Api.Data
import Api.Request.Addresses
import Api.Request.Blocks
import Api.Request.Entities
import Api.Request.Experimental
import Api.Request.General
import Api.Request.MyBulk
import Api.Request.Tags
import Api.Request.Tokens
import Api.Request.Txs
import Api.Time exposing (Posix)
import Dict exposing (Dict)
import Http
import IntDict exposing (IntDict)
import Json.Decode
import Json.Encode
import Model.Direction exposing (Direction(..))
import Model.Graph.Id as Id exposing (AddressId, currency)
import Model.Graph.Layer as Layer exposing (Layer)


type Effect msg
    = SearchEffect
        { query : String
        , currency : Maybe String
        , limit : Maybe Int
        }
        (Api.Data.SearchResult -> msg)
    | GetStatisticsEffect (Api.Data.Stats -> msg)
    | GetConceptsEffect String (List Api.Data.Concept -> msg)
    | ListSupportedTokensEffect String (Api.Data.TokenConfigs -> msg)
    | GetAddressEffect
        { currency : String
        , address : String
        , includeActors : Bool
        }
        (Api.Data.Address -> msg)
    | GetEntityEffect
        { currency : String
        , entity : Int
        }
        (Api.Data.Entity -> msg)
    | GetEntityEffectWithDetails
        { currency : String
        , entity : Int
        , includeActors : Bool
        , includeBestTag : Bool
        }
        (Api.Data.Entity -> msg)
    | GetActorEffect
        { actorId : String
        }
        (Api.Data.Actor -> msg)
    | GetBlockEffect
        { currency : String
        , height : Int
        }
        (Api.Data.Block -> msg)
    | GetBlockByDateEffect
        { currency : String
        , datetime : Posix
        }
        (Api.Data.BlockAtDate -> msg)
    | GetEntityForAddressEffect
        { currency : String
        , address : String
        }
        (Api.Data.Entity -> msg)
    | GetEntityNeighborsEffect
        { currency : String
        , entity : Int
        , isOutgoing : Bool
        , onlyIds : Maybe (List Int)
        , includeLabels : Bool
        , pagesize : Int
        , nextpage : Maybe String
        }
        (Api.Data.NeighborEntities -> msg)
    | GetAddressNeighborsEffect
        { currency : String
        , address : String
        , isOutgoing : Bool
        , onlyIds : Maybe (List String)
        , includeLabels : Bool
        , includeActors : Bool
        , pagesize : Int
        , nextpage : Maybe String
        }
        (Api.Data.NeighborAddresses -> msg)
    | GetAddressTxsEffect
        { currency : String
        , address : String
        , direction : Maybe Direction
        , minHeight : Maybe Int
        , maxHeight : Maybe Int
        , order : Maybe Api.Request.Addresses.Order_
        , pagesize : Int
        , nextpage : Maybe String
        }
        (Api.Data.AddressTxs -> msg)
    | GetEntityAddressesEffect
        { currency : String
        , entity : Int
        , pagesize : Int
        , nextpage : Maybe String
        }
        (Api.Data.EntityAddresses -> msg)
    | GetEntityTxsEffect
        { currency : String
        , entity : Int
        , pagesize : Int
        , nextpage : Maybe String
        }
        (Api.Data.AddressTxs -> msg)
    | GetAddressTagsEffect
        { currency : String
        , address : String
        , pagesize : Int
        , nextpage : Maybe String
        }
        (Api.Data.AddressTags -> msg)
    | GetAddressTagSummaryEffect
        { currency : String
        , address : String
        , includeBestClusterTag : Bool
        }
        (Api.Data.TagSummary -> msg)
    | GetActorTagsEffect
        { actorId : String
        , pagesize : Int
        , nextpage : Maybe String
        }
        (Api.Data.AddressTags -> msg)
    | GetBlockTxsEffect
        { currency : String
        , block : Int
        , pagesize : Int
        , nextpage : Maybe String
        }
        (List Api.Data.Tx -> msg)
    | GetEntityAddressTagsEffect
        { currency : String
        , entity : Int
        , pagesize : Int
        , nextpage : Maybe String
        }
        (Api.Data.AddressTags -> msg)
    | SearchEntityNeighborsEffect
        { currency : String
        , entity : Int
        , isOutgoing : Bool
        , key : Api.Request.Entities.Key
        , value : List String
        , depth : Int
        , breadth : Int
        , maxAddresses : Int
        }
        (List Api.Data.SearchResultLevel1 -> msg)
    | GetTxEffect
        { currency : String
        , txHash : String
        , tokenTxId : Maybe Int
        , includeIo : Bool
        }
        (Api.Data.Tx -> msg)
    | GetTxUtxoAddressesEffect
        { currency : String
        , txHash : String
        , isOutgoing : Bool
        }
        (List Api.Data.TxValue -> msg)
    | ListSpendingTxRefsEffect
        { currency : String
        , txHash : String
        , index : Maybe Int
        }
        (List Api.Data.TxRef -> msg)
    | ListSpentInTxRefsEffect
        { currency : String
        , txHash : String
        , index : Maybe Int
        }
        (List Api.Data.TxRef -> msg)
    | ListAddressTagsEffect
        { label : String
        , nextpage : Maybe String
        , pagesize : Maybe Int
        }
        (Api.Data.AddressTags -> msg)
    | GetAddresslinkTxsEffect
        { currency : String
        , source : String
        , target : String
        , minHeight : Maybe Int
        , maxHeight : Maybe Int
        , order : Maybe Api.Request.Addresses.Order_
        , nextpage : Maybe String
        , pagesize : Int
        }
        (Api.Data.Links -> msg)
    | GetEntitylinkTxsEffect
        { currency : String
        , source : Int
        , target : Int
        , minHeight : Maybe Int
        , maxHeight : Maybe Int
        , order : Maybe Api.Request.Entities.Order_
        , nextpage : Maybe String
        , pagesize : Int
        }
        (Api.Data.Links -> msg)
    | GetTokenTxsEffect
        { currency : String
        , txHash : String
        }
        (List Api.Data.TxAccount -> msg)
    | BulkGetAddressEffect
        { currency : String
        , addresses : List String
        }
        (List Api.Data.Address -> msg)
    | BulkGetAddressTagsEffect
        { currency : String
        , addresses : List String
        , pagesize : Maybe Int
        , includeBestClusterTag : Bool
        }
        (List ( ( String, String ), Maybe Api.Data.AddressTag ) -> msg)
    | BulkGetEntityEffect
        { currency : String
        , entities : List Int
        }
        (List Api.Data.Entity -> msg)
    | BulkGetAddressEntityEffect
        { currency : String
        , addresses : List String
        }
        (List ( String, Api.Data.Entity ) -> msg)
    | BulkGetEntityNeighborsEffect
        { currency : String
        , isOutgoing : Bool
        , entities : List Int
        , onlyIds : Bool
        }
        (List ( Int, Api.Data.NeighborEntity ) -> msg)
    | BulkGetAddressNeighborsEffect
        { currency : String
        , isOutgoing : Bool
        , addresses : List String
        , onlyIds : Bool
        }
        (List ( String, Api.Data.NeighborAddress ) -> msg)


getEntityEgonet :
    { currency : String, entity : Int }
    -> (String -> Int -> Bool -> Api.Data.NeighborEntities -> msg)
    -> IntDict Layer
    -> List (Effect msg)
getEntityEgonet { currency, entity } msg layers =
    let
        -- TODO optimize which only_ids to get for which direction
        onlyIds =
            layers
                |> Layer.entities
                |> List.map (.entity >> .entity)

        effect isOut =
            msg currency entity isOut
                |> GetEntityNeighborsEffect
                    { currency = currency
                    , entity = entity
                    , isOutgoing = isOut
                    , onlyIds = Just onlyIds
                    , pagesize = max 1 <| List.length onlyIds
                    , nextpage = Nothing
                    , includeLabels = False
                    }
    in
    [ effect True
    , effect False
    ]


getAddressEgonet :
    AddressId
    -> (AddressId -> Bool -> Api.Data.NeighborAddresses -> msg)
    -> IntDict Layer
    -> List (Effect msg)
getAddressEgonet id msg layers =
    let
        -- TODO optimize which only_ids to get for which direction
        onlyIds =
            layers
                |> Layer.addresses
                |> List.filter (.address >> .currency >> (==) (Id.currency id))
                |> List.map (.address >> .address)

        effect isOut =
            msg id isOut
                |> GetAddressNeighborsEffect
                    { currency = Id.currency id
                    , address = Id.addressId id
                    , isOutgoing = isOut
                    , onlyIds = Just onlyIds
                    , pagesize = max 1 <| List.length onlyIds
                    , nextpage = Nothing
                    , includeLabels = False
                    , includeActors = True
                    }
    in
    [ effect True
    , effect False
    ]


map : (msgA -> msgB) -> Effect msgA -> Effect msgB
map mapMsg effect =
    case effect of
        GetAddressTagSummaryEffect eff m ->
            m
                >> mapMsg
                |> GetAddressTagSummaryEffect eff

        SearchEffect eff m ->
            m
                >> mapMsg
                |> SearchEffect eff

        GetStatisticsEffect m ->
            m
                >> mapMsg
                |> GetStatisticsEffect

        GetConceptsEffect eff m ->
            m
                >> mapMsg
                |> GetConceptsEffect eff

        ListSupportedTokensEffect eff m ->
            m
                >> mapMsg
                |> ListSupportedTokensEffect eff

        GetAddressEffect eff m ->
            m
                >> mapMsg
                |> GetAddressEffect eff

        GetEntityEffect eff m ->
            m
                >> mapMsg
                |> GetEntityEffect eff

        GetEntityEffectWithDetails eff m ->
            m
                >> mapMsg
                |> GetEntityEffectWithDetails eff

        GetActorEffect eff m ->
            m
                >> mapMsg
                |> GetActorEffect eff

        GetBlockEffect eff m ->
            m
                >> mapMsg
                |> GetBlockEffect eff

        GetBlockByDateEffect eff m ->
            m
                >> mapMsg
                |> GetBlockByDateEffect eff

        GetEntityForAddressEffect eff m ->
            m
                >> mapMsg
                |> GetEntityForAddressEffect eff

        GetEntityNeighborsEffect eff m ->
            m
                >> mapMsg
                |> GetEntityNeighborsEffect eff

        GetAddressNeighborsEffect eff m ->
            m
                >> mapMsg
                |> GetAddressNeighborsEffect eff

        GetAddressTxsEffect eff m ->
            m
                >> mapMsg
                |> GetAddressTxsEffect eff

        GetEntityAddressesEffect eff m ->
            m
                >> mapMsg
                |> GetEntityAddressesEffect eff

        GetEntityTxsEffect eff m ->
            m
                >> mapMsg
                |> GetEntityTxsEffect eff

        GetAddressTagsEffect eff m ->
            m
                >> mapMsg
                |> GetAddressTagsEffect eff

        GetActorTagsEffect eff m ->
            m
                >> mapMsg
                |> GetActorTagsEffect eff

        GetBlockTxsEffect eff m ->
            m
                >> mapMsg
                |> GetBlockTxsEffect eff

        GetEntityAddressTagsEffect eff m ->
            m
                >> mapMsg
                |> GetEntityAddressTagsEffect eff

        SearchEntityNeighborsEffect eff m ->
            m
                >> mapMsg
                |> SearchEntityNeighborsEffect eff

        GetTxEffect eff m ->
            m
                >> mapMsg
                |> GetTxEffect eff

        GetTxUtxoAddressesEffect eff m ->
            m
                >> mapMsg
                |> GetTxUtxoAddressesEffect eff

        ListSpendingTxRefsEffect eff m ->
            m
                >> mapMsg
                |> ListSpendingTxRefsEffect eff

        ListSpentInTxRefsEffect eff m ->
            m
                >> mapMsg
                |> ListSpentInTxRefsEffect eff

        ListAddressTagsEffect eff m ->
            m
                >> mapMsg
                |> ListAddressTagsEffect eff

        GetAddresslinkTxsEffect eff m ->
            m
                >> mapMsg
                |> GetAddresslinkTxsEffect eff

        GetEntitylinkTxsEffect eff m ->
            m
                >> mapMsg
                |> GetEntitylinkTxsEffect eff

        GetTokenTxsEffect eff m ->
            m
                >> mapMsg
                |> GetTokenTxsEffect eff

        BulkGetAddressEffect eff m ->
            m
                >> mapMsg
                |> BulkGetAddressEffect eff

        BulkGetAddressTagsEffect eff m ->
            m
                >> mapMsg
                |> BulkGetAddressTagsEffect eff

        BulkGetEntityEffect eff m ->
            m
                >> mapMsg
                |> BulkGetEntityEffect eff

        BulkGetAddressEntityEffect eff m ->
            m
                >> mapMsg
                |> BulkGetAddressEntityEffect eff

        BulkGetEntityNeighborsEffect eff m ->
            m
                >> mapMsg
                |> BulkGetEntityNeighborsEffect eff

        BulkGetAddressNeighborsEffect eff m ->
            m
                >> mapMsg
                |> BulkGetAddressNeighborsEffect eff


perform : String -> (Result ( Http.Error, Effect msg ) ( Dict String String, msg ) -> msg) -> Effect msg -> Cmd msg
perform apiKey wrapMsg effect =
    case effect of
        GetAddressTagSummaryEffect { currency, address, includeBestClusterTag } toMsg ->
            Api.Request.Experimental.getTagSummaryByAddress currency address (Just includeBestClusterTag)
                |> send apiKey wrapMsg effect toMsg

        SearchEffect { query, currency, limit } toMsg ->
            Api.Request.General.search query currency limit
                |> Api.withTracker "search"
                |> send apiKey wrapMsg effect toMsg

        GetStatisticsEffect toMsg ->
            Api.Request.General.getStatistics
                |> send apiKey wrapMsg effect toMsg

        GetConceptsEffect taxonomy toMsg ->
            Api.Request.Tags.listConcepts taxonomy
                |> send apiKey wrapMsg effect toMsg

        ListSupportedTokensEffect currency toMsg ->
            Api.Request.Tokens.listSupportedTokens currency
                |> send apiKey wrapMsg effect toMsg

        GetEntityNeighborsEffect { currency, entity, isOutgoing, pagesize, onlyIds, nextpage } toMsg ->
            let
                direction =
                    isOutgoingToDirection isOutgoing
            in
            Api.Request.Entities.listEntityNeighbors currency entity direction onlyIds (Just False) (Just False) (Just True) nextpage (Just pagesize)
                |> send apiKey wrapMsg effect toMsg

        GetAddressNeighborsEffect { currency, address, isOutgoing, onlyIds, pagesize, includeLabels, includeActors, nextpage } toMsg ->
            let
                direction =
                    case isOutgoing of
                        True ->
                            Api.Request.Addresses.DirectionOut

                        False ->
                            Api.Request.Addresses.DirectionIn
            in
            Api.Request.Addresses.listAddressNeighbors currency address direction onlyIds (Just includeLabels) (Just includeActors) nextpage (Just pagesize)
                |> send apiKey wrapMsg effect toMsg

        GetAddressEffect { currency, address, includeActors } toMsg ->
            Api.Request.Addresses.getAddress currency address  (Just includeActors)
                |> send apiKey wrapMsg effect toMsg

        GetEntityEffect { currency, entity } toMsg ->
            Api.Request.Entities.getEntity currency entity (Just False) (Just True)
                |> send apiKey wrapMsg effect toMsg

        GetEntityEffectWithDetails { currency, entity, includeActors, includeBestTag } toMsg ->
            Api.Request.Entities.getEntity currency entity (Just (not includeBestTag)) (Just includeActors)
                |> send apiKey wrapMsg effect toMsg

        GetActorEffect { actorId } toMsg ->
            Api.Request.Tags.getActor actorId
                |> send apiKey wrapMsg effect toMsg

        GetBlockEffect { currency, height } toMsg ->
            Api.Request.Blocks.getBlock currency height
                |> send apiKey wrapMsg effect toMsg

        GetBlockByDateEffect { currency, datetime } toMsg ->
            Api.Request.Blocks.getBlockByDate currency datetime
                |> send apiKey wrapMsg effect toMsg

        GetEntityForAddressEffect { currency, address } toMsg ->
            Api.Request.Addresses.getAddressEntity currency address
                |> send apiKey wrapMsg effect toMsg

        GetAddressTxsEffect { currency, address, direction, minHeight, maxHeight, order, pagesize, nextpage } toMsg ->
            let
                dir =
                    case direction of
                        Nothing ->
                            Nothing

                        Just Incoming ->
                            Just Api.Request.Addresses.DirectionIn

                        Just Outgoing ->
                            Just Api.Request.Addresses.DirectionOut
            in
            -- currency_path address_path neighbor_query minHeight_query maxHeight_query order_query page_query pagesize_query
            Api.Request.Addresses.listAddressTxs currency address dir minHeight maxHeight order Nothing nextpage (Just pagesize)
                |> send apiKey wrapMsg effect toMsg

        ListSpendingTxRefsEffect { currency, txHash, index } toMsg ->
            Api.Request.Txs.getSpendingTxs currency txHash index
                |> send apiKey wrapMsg effect toMsg

        ListSpentInTxRefsEffect { currency, txHash, index } toMsg ->
            Api.Request.Txs.getSpentInTxs currency txHash index
                |> send apiKey wrapMsg effect toMsg

        GetAddresslinkTxsEffect { currency, source, target, minHeight, maxHeight, order, pagesize, nextpage } toMsg ->
            Api.Request.Addresses.listAddressLinks currency source target minHeight maxHeight order nextpage (Just pagesize)
                |> send apiKey wrapMsg effect toMsg

        GetEntitylinkTxsEffect { currency, source, target, minHeight, maxHeight, pagesize, nextpage, order } toMsg ->
            Api.Request.Entities.listEntityLinks currency source target minHeight maxHeight order nextpage (Just pagesize)
                |> send apiKey wrapMsg effect toMsg

        GetAddressTagsEffect { currency, address, pagesize, nextpage } toMsg ->
            Api.Request.Addresses.listTagsByAddress currency address nextpage (Just pagesize) (Just False)
                |> send apiKey wrapMsg effect toMsg

        GetActorTagsEffect { actorId, pagesize, nextpage } toMsg ->
            Api.Request.Tags.getActorTags actorId nextpage (Just pagesize)
                |> send apiKey wrapMsg effect toMsg

        GetEntityAddressTagsEffect { currency, entity, pagesize, nextpage } toMsg ->
            Api.Request.Entities.listAddressTagsByEntity currency entity nextpage (Just pagesize)
                |> send apiKey wrapMsg effect toMsg

        GetEntityAddressesEffect { currency, entity, pagesize, nextpage } toMsg ->
            Api.Request.Entities.listEntityAddresses currency entity nextpage (Just pagesize)
                |> send apiKey wrapMsg effect toMsg

        GetEntityTxsEffect { currency, entity, pagesize, nextpage } toMsg ->
            Api.Request.Entities.listEntityTxs currency entity Nothing Nothing Nothing Nothing Nothing nextpage (Just pagesize)
                |> send apiKey wrapMsg effect toMsg

        GetBlockTxsEffect { currency, block } toMsg ->
            Api.Request.Blocks.listBlockTxs currency block
                |> send apiKey wrapMsg effect toMsg

        GetTxEffect { currency, txHash, tokenTxId, includeIo } toMsg ->
            Api.Request.Txs.getTx currency txHash (Just includeIo) tokenTxId
                |> send apiKey wrapMsg effect toMsg

        GetTxUtxoAddressesEffect { currency, txHash, isOutgoing } toMsg ->
            let
                io =
                    if isOutgoing then
                        Api.Request.Txs.IoOutputs

                    else
                        Api.Request.Txs.IoInputs
            in
            Api.Request.Txs.getTxIo currency txHash io
                |> send apiKey wrapMsg effect toMsg

        SearchEntityNeighborsEffect e toMsg ->
            let
                direction =
                    isOutgoingToDirection e.isOutgoing
            in
            Api.Request.Entities.searchEntityNeighbors e.currency e.entity direction e.key e.value e.depth (Just e.breadth) (Just e.maxAddresses)
                |> send apiKey wrapMsg effect toMsg

        ListAddressTagsEffect { label, nextpage, pagesize } toMsg ->
            Api.Request.Tags.listAddressTags label nextpage pagesize
                |> send apiKey wrapMsg effect toMsg

        GetTokenTxsEffect { currency, txHash } toMsg ->
            Api.Request.Txs.listTokenTxs currency txHash
                |> send apiKey wrapMsg effect toMsg

        BulkGetAddressEffect e toMsg ->
            listWithMaybes Api.Data.addressDecoder
                |> Api.Request.MyBulk.bulkJson
                    e.currency
                    Api.Request.MyBulk.OperationGetAddress
                    (Json.Encode.object
                        [ ( "address", Json.Encode.list Json.Encode.string e.addresses )
                        ]
                    )
                |> send apiKey wrapMsg effect toMsg

        BulkGetAddressTagsEffect e toMsg ->
            Json.Decode.list (Json.Decode.map2 Tuple.pair (Json.Decode.field "_request_address" Json.Decode.string |> Json.Decode.map (Tuple.pair e.currency)) (Json.Decode.maybe Api.Data.addressTagDecoder))
                |> Api.Request.MyBulk.bulkJson
                    e.currency
                    Api.Request.MyBulk.OperationListTagsByAddress
                    (Json.Encode.object
                        [ ( "address", Json.Encode.list Json.Encode.string e.addresses )
                        , ( "pagesize"
                          , e.pagesize
                                |> Maybe.map Json.Encode.int
                                |> Maybe.withDefault Json.Encode.null
                          )
                        , ( "include_best_cluster_tag", Json.Encode.bool e.includeBestClusterTag )
                        ]
                    )
                |> send apiKey wrapMsg effect toMsg

        BulkGetEntityEffect e toMsg ->
            listWithMaybes Api.Data.entityDecoder
                |> Api.Request.MyBulk.bulkJson
                    e.currency
                    Api.Request.MyBulk.OperationGetEntity
                    (Json.Encode.object
                        [ ( "entity", Json.Encode.list Json.Encode.int e.entities )
                        ]
                    )
                |> send apiKey wrapMsg effect toMsg

        BulkGetAddressEntityEffect e toMsg ->
            listWithMaybes
                (Json.Decode.field "_request_address" Json.Decode.string
                    |> Json.Decode.andThen
                        (\requestAddress ->
                            Json.Decode.map
                                (\entity -> ( requestAddress, entity ))
                                Api.Data.entityDecoder
                        )
                )
                |> Api.Request.MyBulk.bulkJson
                    e.currency
                    Api.Request.MyBulk.OperationGetAddressEntity
                    (Json.Encode.object
                        [ ( "address", Json.Encode.list Json.Encode.string e.addresses )
                        ]
                    )
                |> send apiKey wrapMsg effect toMsg

        BulkGetEntityNeighborsEffect e toMsg ->
            listWithMaybes
                (Json.Decode.field "_request_entity" Json.Decode.int
                    |> Json.Decode.andThen
                        (\requestEntity ->
                            Json.Decode.map
                                (\entity -> ( requestEntity, entity ))
                                Api.Data.neighborEntityDecoder
                        )
                )
                |> Api.Request.MyBulk.bulkJson
                    e.currency
                    Api.Request.MyBulk.OperationListEntityNeighbors
                    (Json.Encode.object <|
                        [ ( "entity", Json.Encode.list Json.Encode.int e.entities )
                        , ( "direction"
                          , Json.Encode.string <|
                                Api.Request.Entities.stringFromDirection <|
                                    if e.isOutgoing then
                                        Api.Request.Entities.DirectionOut

                                    else
                                        Api.Request.Entities.DirectionIn
                          )
                        ]
                            ++ (if e.onlyIds then
                                    [ ( "only_ids", Json.Encode.list Json.Encode.int e.entities )
                                    ]

                                else
                                    []
                               )
                    )
                |> send apiKey wrapMsg effect toMsg

        BulkGetAddressNeighborsEffect e toMsg ->
            listWithMaybes
                (Json.Decode.field "_request_address" Json.Decode.string
                    |> Json.Decode.andThen
                        (\requestAddress ->
                            Json.Decode.map
                                (\address -> ( requestAddress, address ))
                                Api.Data.neighborAddressDecoder
                        )
                )
                |> Api.Request.MyBulk.bulkJson
                    e.currency
                    Api.Request.MyBulk.OperationListAddressNeighbors
                    (Json.Encode.object <|
                        [ ( "address", Json.Encode.list Json.Encode.string e.addresses )
                        , ( "direction"
                          , Json.Encode.string <|
                                Api.Request.Entities.stringFromDirection <|
                                    if e.isOutgoing then
                                        Api.Request.Entities.DirectionOut

                                    else
                                        Api.Request.Entities.DirectionIn
                          )
                        ]
                            ++ (if e.onlyIds then
                                    [ ( "only_ids", Json.Encode.list Json.Encode.string e.addresses )
                                    ]

                                else
                                    []
                               )
                    )
                |> send apiKey wrapMsg effect toMsg


withAuthorization : String -> Api.Request a -> Api.Request a
withAuthorization apiKey request =
    if String.isEmpty apiKey then
        request

    else
        Api.withHeader "Authorization" apiKey request


send : String -> (Result ( Http.Error, eff ) ( Dict String String, msg ) -> msg) -> eff -> (a -> msg) -> Api.Request a -> Cmd msg
send apiKey wrapMsg effect toMsg =
    withAuthorization apiKey
        >> Api.sendAndAlsoReceiveHeaders wrapMsg effect toMsg


isOutgoingToDirection : Bool -> Api.Request.Entities.Direction
isOutgoingToDirection isOutgoing =
    if isOutgoing then
        Api.Request.Entities.DirectionOut

    else
        Api.Request.Entities.DirectionIn


isOutgoingToAddressDirection : Bool -> Api.Request.Addresses.Direction
isOutgoingToAddressDirection isOutgoing =
    if isOutgoing then
        Api.Request.Addresses.DirectionOut

    else
        Api.Request.Addresses.DirectionIn


listWithMaybes : Json.Decode.Decoder a -> Json.Decode.Decoder (List a)
listWithMaybes decoder =
    Json.Decode.list (Json.Decode.maybe decoder)
        |> Json.Decode.map (List.filterMap identity)
