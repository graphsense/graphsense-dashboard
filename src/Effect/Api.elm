module Effect.Api exposing (Effect(..), SearchRequestConfig, UserInfo, defaultSearchConfig, effectToTracker, getAddressEgonet, getEntityEgonet, isOutgoingToAddressDirection, isOutgoingToDirection, isUserEndpointConfigured, listWithMaybes, map, perform, retryToken, send, withAuthorization)

import Api
import Api.Data
import Api.Request.Addresses
import Api.Request.Blocks
import Api.Request.Clusters
import Api.Request.Experimental
import Api.Request.General
import Api.Request.MyBulk
import Api.Request.Tags
import Api.Request.Tokens
import Api.Request.Txs
import Api.Time exposing (Posix, dateTimeDecoder)
import Http
import IntDict exposing (IntDict)
import Json.Decode
import Json.Encode
import Json.Encode.Extra as Encode
import Model.Direction exposing (Direction(..))
import Model.Graph.Id as Id exposing (AddressId)
import Model.Graph.Layer as Layer exposing (Layer)
import Model.Pathfinder.Id exposing (Id)
import Sha256
import Task
import Time
import Tuple exposing (pair)
import Util.Http exposing (Headers)


type alias SearchRequestConfig =
    { includeSubTxIdentifiers : Maybe Bool
    , includeLabels : Maybe Bool
    , includeActors : Maybe Bool
    , includeTxs : Maybe Bool
    , includeAddresses : Maybe Bool
    }


type alias UserInfo =
    { expiration : Maybe Time.Posix
    }


defaultSearchConfig : SearchRequestConfig
defaultSearchConfig =
    { includeSubTxIdentifiers = Nothing
    , includeLabels = Nothing
    , includeActors = Nothing
    , includeTxs = Nothing
    , includeAddresses = Nothing
    }


userEndpointUrl : String
userEndpointUrl =
    "{{VITE_GS_USER_ENDPOINT_URL}}"


isUserEndpointConfigured : Bool
isUserEndpointConfigured =
    not (String.isEmpty userEndpointUrl)
        && not (String.contains "{{" userEndpointUrl)


type Effect msg
    = SearchEffect
        { query : String
        , currency : Maybe String
        , limit : Maybe Int
        , config : SearchRequestConfig
        }
        (Api.Data.SearchResult -> msg)
    | GetStatisticsEffect (Api.Data.Stats -> msg)
    | GetConceptsEffect String (List Api.Data.Concept -> msg)
    | ListSupportedTokensEffect String (Api.Data.TokenConfigs -> msg)
    | GetMeEffect (UserInfo -> msg)
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
        (Api.Data.Cluster -> msg)
    | GetEntityEffectWithDetails
        { currency : String
        , entity : Int
        , includeActors : Bool
        , includeBestTag : Bool
        }
        (Api.Data.Cluster -> msg)
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
        (Api.Data.Cluster -> msg)
    | GetEntityNeighborsEffect
        { currency : String
        , entity : Int
        , isOutgoing : Bool
        , onlyIds : Maybe (List Int)
        , includeLabels : Bool
        , pagesize : Int
        , nextpage : Maybe String
        }
        (Api.Data.NeighborClusters -> msg)
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
        , tokenCurrency : Maybe String
        , order : Maybe Api.Request.Addresses.Order_
        , pagesize : Int
        , nextpage : Maybe String
        }
        (Api.Data.AddressTxs -> msg)
    | GetAddressTxsByDateEffect
        { currency : String
        , address : String
        , direction : Maybe Direction
        , minDate : Maybe Time.Posix
        , maxDate : Maybe Time.Posix
        , tokenCurrency : Maybe String
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
        (Api.Data.ClusterAddresses -> msg)
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
        , includeBestClusterTag : Bool
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
        , key : Api.Request.Clusters.Key
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
        , minDate : Maybe Posix
        , maxDate : Maybe Posix
        , tokenCurrency : Maybe String
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
        , order : Maybe Api.Request.Clusters.Order_
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
        (List Api.Data.Cluster -> msg)
    | BulkGetAddressEntityEffect
        { currency : String
        , addresses : List String
        }
        (List ( String, Api.Data.Cluster ) -> msg)
    | BulkGetEntityNeighborsEffect
        { currency : String
        , isOutgoing : Bool
        , entities : List Int
        , onlyIds : Bool
        }
        (List ( Int, Api.Data.NeighborCluster ) -> msg)
    | BulkGetAddressNeighborsEffect
        { currency : String
        , isOutgoing : Bool
        , addresses : List String
        , onlyIds : Maybe (List String)
        }
        (List ( String, Api.Data.NeighborAddress ) -> msg)
    | BulkGetTxEffect
        { currency : String
        , txs : List String
        }
        (List ( String, Api.Data.Tx ) -> msg)
    | BulkGetAddressTagSummaryEffect
        { currency : String
        , addresses : List String
        , includeBestClusterTag : Bool
        }
        (List ( Id, Api.Data.TagSummary ) -> msg)
    | AddUserReportedTag Api.Data.UserReportedTag (Api.Data.UserTagReportResponse -> msg)
    | ListRelatedAddressesEffect
        { currency : String
        , address : String
        , reltype : Api.Request.Addresses.AddressRelationType
        , pagesize : Int
        , nextpage : Maybe String
        }
        (Api.Data.RelatedAddresses -> msg)
    | GetConversionEffect { currency : String, txHash : String } (List Api.Data.ExternalConversion -> msg)
    | ListTxFlowsEffect
        { currency : String
        , txHash : String
        , includeZeroValueSubTxs : Bool
        , token_currency : Maybe String
        , pagesize : Maybe Int
        , nextpage : Maybe String
        }
        (Api.Data.Txs -> msg)
    | CancelEffect String


getEntityEgonet :
    { currency : String, entity : Int }
    -> (String -> Int -> Bool -> Api.Data.NeighborClusters -> msg)
    -> IntDict Layer
    -> List (Effect msg)
getEntityEgonet { currency, entity } msg layers =
    let
        -- TODO optimize which only_ids to get for which direction
        onlyIds =
            layers
                |> Layer.entities
                |> List.map (.entity >> .cluster)

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
        ListTxFlowsEffect eff m ->
            m
                >> mapMsg
                |> ListTxFlowsEffect eff

        AddUserReportedTag eff m ->
            m
                >> mapMsg
                |> AddUserReportedTag eff

        GetAddressTagSummaryEffect eff m ->
            m
                >> mapMsg
                |> GetAddressTagSummaryEffect eff

        BulkGetAddressTagSummaryEffect eff m ->
            m
                >> mapMsg
                |> BulkGetAddressTagSummaryEffect eff

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

        GetMeEffect m ->
            m
                >> mapMsg
                |> GetMeEffect

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

        GetAddressTxsByDateEffect eff m ->
            m
                >> mapMsg
                |> GetAddressTxsByDateEffect eff

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

        BulkGetTxEffect eff m ->
            m
                >> mapMsg
                |> BulkGetTxEffect eff

        ListRelatedAddressesEffect eff m ->
            m
                >> mapMsg
                |> ListRelatedAddressesEffect eff

        GetConversionEffect eff m ->
            m
                >> mapMsg
                |> GetConversionEffect eff

        CancelEffect s ->
            CancelEffect s


perform : String -> (Result ( Http.Error, Headers, Effect msg ) ( Headers, msg ) -> msg) -> (String -> msg) -> Effect msg -> Cmd msg
perform apiKey wrapMsg cancelMsg effect =
    let
        withTracker =
            effectToTracker effect
                |> Maybe.map Api.withTracker
                |> Maybe.withDefault identity
    in
    case effect of
        AddUserReportedTag data toMsg ->
            Api.Request.Tags.reportTag data |> send apiKey wrapMsg effect toMsg

        GetAddressTagSummaryEffect { currency, address, includeBestClusterTag } toMsg ->
            Api.Request.Experimental.getTagSummaryByAddress currency address (Just includeBestClusterTag)
                |> send apiKey wrapMsg effect toMsg

        SearchEffect { query, currency, limit, config } toMsg ->
            Api.Request.General.search query currency limit config.includeSubTxIdentifiers config.includeLabels config.includeActors config.includeTxs config.includeAddresses
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

        GetMeEffect toMsg ->
            if isUserEndpointConfigured then
                Api.request "GET" userEndpointUrl [] [] [] Nothing userInfoDecoder
                    |> send apiKey wrapMsg effect toMsg

            else
                Cmd.none

        GetEntityNeighborsEffect { currency, entity, isOutgoing, pagesize, onlyIds, nextpage } toMsg ->
            let
                direction =
                    isOutgoingToDirection isOutgoing
            in
            Api.Request.Clusters.listClusterNeighbors currency entity direction onlyIds (Just False) (Just False) (Just True) nextpage (Just pagesize)
                |> send apiKey wrapMsg effect toMsg

        GetAddressNeighborsEffect { currency, address, isOutgoing, onlyIds, pagesize, includeLabels, includeActors, nextpage } toMsg ->
            let
                direction =
                    if isOutgoing then
                        Api.Request.Addresses.DirectionOut

                    else
                        Api.Request.Addresses.DirectionIn
            in
            Api.Request.Addresses.listAddressNeighbors currency address direction onlyIds (Just includeLabels) (Just includeActors) nextpage (Just pagesize)
                |> withTracker
                |> send apiKey wrapMsg effect toMsg

        GetAddressEffect { currency, address, includeActors } toMsg ->
            Api.Request.Addresses.getAddress currency address (Just includeActors)
                |> send apiKey wrapMsg effect toMsg

        GetEntityEffect { currency, entity } toMsg ->
            Api.Request.Clusters.getCluster currency entity (Just False) (Just True)
                |> send apiKey wrapMsg effect toMsg

        GetEntityEffectWithDetails { currency, entity, includeActors, includeBestTag } toMsg ->
            Api.Request.Clusters.getCluster currency entity (Just (not includeBestTag)) (Just includeActors)
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
            Api.Request.Addresses.getAddressEntity currency address Nothing
                |> send apiKey wrapMsg effect toMsg

        GetAddressTxsEffect { currency, address, direction, minHeight, maxHeight, order, tokenCurrency, pagesize, nextpage } toMsg ->
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
            Api.Request.Addresses.listAddressTxs currency address dir minHeight maxHeight Nothing Nothing order tokenCurrency nextpage (Just pagesize)
                |> send apiKey wrapMsg effect toMsg

        GetAddressTxsByDateEffect { currency, address, direction, minDate, maxDate, order, tokenCurrency, pagesize, nextpage } toMsg ->
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
            Api.Request.Addresses.listAddressTxs currency address dir Nothing Nothing minDate maxDate order tokenCurrency nextpage (Just pagesize)
                |> withTracker
                |> send apiKey wrapMsg effect toMsg

        ListSpendingTxRefsEffect { currency, txHash, index } toMsg ->
            Api.Request.Txs.getSpendingTxs currency txHash index
                |> send apiKey wrapMsg effect toMsg

        ListSpentInTxRefsEffect { currency, txHash, index } toMsg ->
            Api.Request.Txs.getSpentInTxs currency txHash index
                |> send apiKey wrapMsg effect toMsg

        GetAddresslinkTxsEffect { currency, source, target, minHeight, maxHeight, minDate, maxDate, tokenCurrency, order, pagesize, nextpage } toMsg ->
            Api.Request.Addresses.listAddressLinks currency source target minHeight maxHeight minDate maxDate order tokenCurrency nextpage (Just pagesize)
                |> withTracker
                |> send apiKey wrapMsg effect toMsg

        GetEntitylinkTxsEffect { currency, source, target, minHeight, maxHeight, pagesize, nextpage, order } toMsg ->
            Api.Request.Clusters.listClusterLinks currency source target minHeight maxHeight Nothing Nothing order Nothing nextpage (Just pagesize)
                |> send apiKey wrapMsg effect toMsg

        GetAddressTagsEffect { currency, address, pagesize, nextpage, includeBestClusterTag } toMsg ->
            Api.Request.Addresses.listTagsByAddress currency address nextpage (Just pagesize) (Just includeBestClusterTag)
                |> withTracker
                |> send apiKey wrapMsg effect toMsg

        GetActorTagsEffect { actorId, pagesize, nextpage } toMsg ->
            Api.Request.Tags.getActorTags actorId nextpage (Just pagesize)
                |> send apiKey wrapMsg effect toMsg

        GetEntityAddressTagsEffect { currency, entity, pagesize, nextpage } toMsg ->
            Api.Request.Clusters.listAddressTagsByCluster currency entity nextpage (Just pagesize)
                |> withTracker
                |> send apiKey wrapMsg effect toMsg

        GetEntityAddressesEffect { currency, entity, pagesize, nextpage } toMsg ->
            Api.Request.Clusters.listClusterAddresses currency entity nextpage (Just pagesize)
                |> withTracker
                |> send apiKey wrapMsg effect toMsg

        GetEntityTxsEffect { currency, entity, pagesize, nextpage } toMsg ->
            Api.Request.Clusters.listClusterTxs currency entity Nothing Nothing Nothing Nothing Nothing Nothing Nothing nextpage (Just pagesize)
                |> send apiKey wrapMsg effect toMsg

        GetBlockTxsEffect { currency, block } toMsg ->
            Api.Request.Blocks.listBlockTxs currency block
                |> send apiKey wrapMsg effect toMsg

        GetTxEffect { currency, txHash, tokenTxId, includeIo } toMsg ->
            let
                includeHeuristics =
                    if includeIo then
                        Just
                            [ Api.Request.Txs.IncludeHeuristicAll
                            ]

                    else
                        Nothing

                includeIoIndex =
                    if includeIo then
                        Just True

                    else
                        Nothing
            in
            Api.Request.Txs.getTx currency txHash (Just includeIo) Nothing includeIoIndex tokenTxId includeHeuristics
                |> send apiKey wrapMsg effect toMsg

        GetTxUtxoAddressesEffect { currency, txHash, isOutgoing } toMsg ->
            let
                io =
                    if isOutgoing then
                        Api.Request.Txs.IoOutputs

                    else
                        Api.Request.Txs.IoInputs
            in
            Api.Request.Txs.getTxIo currency txHash io Nothing Nothing
                |> send apiKey wrapMsg effect toMsg

        SearchEntityNeighborsEffect e toMsg ->
            let
                direction =
                    isOutgoingToDirection e.isOutgoing
            in
            Api.Request.Clusters.searchClusterNeighbors e.currency e.entity direction e.key e.value e.depth (Just e.breadth) (Just e.maxAddresses)
                |> send apiKey wrapMsg effect toMsg

        ListAddressTagsEffect { label, nextpage, pagesize } toMsg ->
            Api.Request.Tags.listAddressTags label nextpage pagesize
                |> send apiKey wrapMsg effect toMsg

        GetTokenTxsEffect { currency, txHash } toMsg ->
            Api.Request.Txs.listTokenTxs currency txHash
                |> send apiKey wrapMsg effect toMsg

        BulkGetAddressEffect e toMsg ->
            if List.isEmpty e.addresses then
                []
                    |> Task.succeed
                    |> Task.perform toMsg

            else
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
            if List.isEmpty e.addresses then
                []
                    |> Task.succeed
                    |> Task.perform toMsg

            else
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
            if List.isEmpty e.entities then
                []
                    |> Task.succeed
                    |> Task.perform toMsg

            else
                listWithMaybes Api.Data.clusterDecoder
                    |> Api.Request.MyBulk.bulkJson
                        e.currency
                        Api.Request.MyBulk.OperationGetEntity
                        (Json.Encode.object
                            [ ( "entity", Json.Encode.list Json.Encode.int e.entities )
                            ]
                        )
                    |> send apiKey wrapMsg effect toMsg

        BulkGetAddressEntityEffect e toMsg ->
            if List.isEmpty e.addresses then
                []
                    |> Task.succeed
                    |> Task.perform toMsg

            else
                listWithMaybes
                    (Json.Decode.field "_request_address" Json.Decode.string
                        |> Json.Decode.andThen
                            (\requestAddress ->
                                Json.Decode.map
                                    (\entity -> ( requestAddress, entity ))
                                    Api.Data.clusterDecoder
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
            if List.isEmpty e.entities then
                []
                    |> Task.succeed
                    |> Task.perform toMsg

            else
                listWithMaybes
                    (Json.Decode.field "_request_entity" Json.Decode.int
                        |> Json.Decode.andThen
                            (\requestEntity ->
                                Json.Decode.map
                                    (\entity -> ( requestEntity, entity ))
                                    Api.Data.neighborClusterDecoder
                            )
                    )
                    |> Api.Request.MyBulk.bulkJson
                        e.currency
                        Api.Request.MyBulk.OperationListEntityNeighbors
                        (Json.Encode.object <|
                            [ ( "entity", Json.Encode.list Json.Encode.int e.entities )
                            , ( "direction"
                              , Json.Encode.string <|
                                    Api.Request.Clusters.stringFromDirection <|
                                        if e.isOutgoing then
                                            Api.Request.Clusters.DirectionOut

                                        else
                                            Api.Request.Clusters.DirectionIn
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
            if List.isEmpty e.addresses then
                []
                    |> Task.succeed
                    |> Task.perform toMsg

            else
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
                                    Api.Request.Clusters.stringFromDirection <|
                                        if e.isOutgoing then
                                            Api.Request.Clusters.DirectionOut

                                        else
                                            Api.Request.Clusters.DirectionIn
                              )
                            ]
                                ++ (e.onlyIds
                                        |> Maybe.map
                                            (Json.Encode.list Json.Encode.string
                                                >> pair "only_ids"
                                                >> List.singleton
                                            )
                                        |> Maybe.withDefault []
                                   )
                        )
                    |> send apiKey wrapMsg effect toMsg

        BulkGetTxEffect e toMsg ->
            if List.isEmpty e.txs then
                []
                    |> Task.succeed
                    |> Task.perform toMsg

            else
                listWithMaybes
                    (Json.Decode.field "_request_tx_hash" Json.Decode.string
                        |> Json.Decode.andThen
                            (\requestTxHash ->
                                Json.Decode.map
                                    (\tx -> ( requestTxHash, tx ))
                                    Api.Data.txDecoder
                            )
                    )
                    |> Api.Request.MyBulk.bulkJson
                        e.currency
                        Api.Request.MyBulk.OperationGetTx
                        (Json.Encode.object
                            [ ( "tx_hash", Json.Encode.list Json.Encode.string e.txs )
                            , ( "include_io", Json.Encode.bool True )
                            ]
                        )
                    |> send apiKey wrapMsg effect toMsg

        BulkGetAddressTagSummaryEffect { currency, addresses, includeBestClusterTag } toMsg ->
            if List.isEmpty addresses then
                []
                    |> Task.succeed
                    |> Task.perform toMsg

            else
                Json.Decode.list
                    (Json.Decode.field "_request_address" Json.Decode.string
                        |> Json.Decode.andThen
                            (\requestAddress ->
                                Json.Decode.map
                                    (\ts -> ( ( currency, requestAddress ), ts ))
                                    Api.Data.tagSummaryDecoder
                                    |> Json.Decode.maybe
                            )
                    )
                    |> Json.Decode.map (List.filterMap identity)
                    |> Api.Request.MyBulk.bulkJson
                        currency
                        Api.Request.MyBulk.OperationGetAddressTagSummary
                        (Json.Encode.object
                            [ ( "address", Json.Encode.list Json.Encode.string addresses )
                            , ( "include_best_cluster_tag", Json.Encode.bool includeBestClusterTag )
                            ]
                        )
                    |> send apiKey wrapMsg effect toMsg

        ListRelatedAddressesEffect { currency, address, reltype, pagesize, nextpage } toMsg ->
            Api.Request.Addresses.listRelatedAddresses currency address (Just reltype) nextpage (Just pagesize)
                |> send apiKey wrapMsg effect toMsg

        GetConversionEffect { currency, txHash } toMsg ->
            Api.Request.Txs.getTxConversions currency txHash
                |> send apiKey wrapMsg effect toMsg

        ListTxFlowsEffect { currency, txHash, includeZeroValueSubTxs, token_currency, pagesize, nextpage } toMsg ->
            Api.Request.Txs.listTxFlows currency txHash (Just (not includeZeroValueSubTxs)) Nothing token_currency nextpage pagesize
                |> send apiKey wrapMsg effect toMsg

        CancelEffect tracker ->
            [ Http.cancel tracker
            , Task.succeed ()
                |> Task.perform (\_ -> cancelMsg tracker)
            ]
                |> Cmd.batch


effectToTracker : Effect msg -> Maybe String
effectToTracker effect =
    let
        encodePosix =
            Time.posixToMillis >> Json.Encode.int
    in
    case effect of
        GetAddressTxsByDateEffect { currency, address, direction, minDate, maxDate, order, tokenCurrency, pagesize, nextpage } _ ->
            "GetAddressTxsByDateEffect"
                ++ ([ Json.Encode.string currency
                    , Json.Encode.string address
                    , Encode.maybe (Model.Direction.toString >> Json.Encode.string) direction
                    , Encode.maybe encodePosix minDate
                    , Encode.maybe encodePosix maxDate
                    , Encode.maybe (Api.Request.Addresses.stringFromOrder_ >> Json.Encode.string) order
                    , Encode.maybe Json.Encode.string tokenCurrency
                    , Encode.maybe Json.Encode.string nextpage
                    , Json.Encode.int pagesize
                    ]
                        |> Json.Encode.list identity
                        |> Json.Encode.encode 0
                   )
                |> Just

        GetAddresslinkTxsEffect { currency, source, target, minDate, maxDate, minHeight, maxHeight, order, tokenCurrency, pagesize, nextpage } _ ->
            "GetAddresslinkTxsEffect"
                ++ ([ Json.Encode.string currency
                    , Json.Encode.string source
                    , Json.Encode.string target
                    , Encode.maybe encodePosix minDate
                    , Encode.maybe encodePosix maxDate
                    , Encode.maybe Json.Encode.int minHeight
                    , Encode.maybe Json.Encode.int maxHeight
                    , Encode.maybe (Api.Request.Addresses.stringFromOrder_ >> Json.Encode.string) order
                    , Encode.maybe Json.Encode.string tokenCurrency
                    , Encode.maybe Json.Encode.string nextpage
                    , Json.Encode.int pagesize
                    ]
                        |> Json.Encode.list identity
                        |> Json.Encode.encode 0
                   )
                |> Just

        GetAddressNeighborsEffect { currency, address, isOutgoing, onlyIds, pagesize, includeLabels, includeActors, nextpage } _ ->
            "GetAddressNeighborsEffect"
                ++ ([ Json.Encode.string currency
                    , Json.Encode.string address
                    , Json.Encode.bool isOutgoing
                    , Encode.maybe (Json.Encode.list Json.Encode.string) onlyIds
                    , Json.Encode.bool includeLabels
                    , Json.Encode.bool includeActors
                    , Encode.maybe Json.Encode.string nextpage
                    , Json.Encode.int pagesize
                    ]
                        |> Json.Encode.list identity
                        |> Json.Encode.encode 0
                   )
                |> Just

        GetEntityAddressesEffect { currency, entity, pagesize, nextpage } _ ->
            "GetEntityAddressesEffect"
                ++ ([ Json.Encode.string currency
                    , Json.Encode.int entity
                    , Encode.maybe Json.Encode.string nextpage
                    , Json.Encode.int pagesize
                    ]
                        |> Json.Encode.list identity
                        |> Json.Encode.encode 0
                   )
                |> Just

        GetEntityAddressTagsEffect { currency, entity, pagesize, nextpage } _ ->
            "GetEntityAddressTagsEffect"
                ++ ([ Json.Encode.string currency
                    , Json.Encode.int entity
                    , Encode.maybe Json.Encode.string nextpage
                    , Json.Encode.int pagesize
                    ]
                        |> Json.Encode.list identity
                        |> Json.Encode.encode 0
                   )
                |> Just

        GetAddressTagsEffect { currency, address, pagesize, nextpage } _ ->
            "GetAddressTagsEffect"
                ++ ([ Json.Encode.string currency
                    , Json.Encode.string address
                    , Encode.maybe Json.Encode.string nextpage
                    , Json.Encode.int pagesize
                    ]
                        |> Json.Encode.list identity
                        |> Json.Encode.encode 0
                   )
                |> Just

        _ ->
            Nothing


{-| Stable per-request key used to gate retries on transient HTTP errors and
to index the `model.statusbar.retries` dict. Returning `Nothing` opts the
effect out of the retry mechanism entirely (e.g. mutations, search).

For effects that already carry an `effectToTracker` (HTTP-cancellation
tracker) the same key is reused so that `BrowserCancelledRequest` cleans up
the matching `retries` entry. For other effects the key is a sha256 of a
parameter-bearing fingerprint, which keeps dict keys bounded in size while
still distinguishing simultaneous in-flight requests with different params.

-}
retryToken : Effect msg -> Maybe String
retryToken effect =
    let
        hash s =
            Just (Sha256.sha256 s)

        b x =
            if x then
                "1"

            else
                "0"

        mb f =
            Maybe.map f >> Maybe.withDefault ""

        mbs =
            mb identity

        mbi =
            mb String.fromInt

        i =
            String.fromInt

        p =
            Time.posixToMillis >> String.fromInt

        join =
            String.join "|"

        dir =
            Model.Direction.toString

        ord =
            Api.Request.Addresses.stringFromOrder_

        cord =
            Api.Request.Clusters.stringFromOrder_

        ckey =
            Api.Request.Clusters.stringFromKey

        rel =
            Api.Request.Addresses.stringFromAddressRelationType
    in
    case effect of
        -- Opt-outs: never retried.
        SearchEffect _ _ ->
            Nothing

        GetMeEffect _ ->
            Nothing

        AddUserReportedTag _ _ ->
            Nothing

        CancelEffect _ ->
            Nothing

        -- Effects with an HTTP-cancellation tracker reuse that key so the
        -- cancellation handler clears the matching retries entry.
        GetAddressTxsByDateEffect _ _ ->
            effectToTracker effect

        GetAddresslinkTxsEffect _ _ ->
            effectToTracker effect

        GetAddressNeighborsEffect _ _ ->
            effectToTracker effect

        GetEntityAddressesEffect _ _ ->
            effectToTracker effect

        GetEntityAddressTagsEffect _ _ ->
            effectToTracker effect

        GetAddressTagsEffect _ _ ->
            effectToTracker effect

        -- Everything else: sha256 over a parameter-bearing fingerprint.
        GetStatisticsEffect _ ->
            hash "GetStatisticsEffect"

        GetConceptsEffect taxonomy _ ->
            hash (join [ "GetConceptsEffect", taxonomy ])

        ListSupportedTokensEffect currency _ ->
            hash (join [ "ListSupportedTokensEffect", currency ])

        GetAddressEffect { currency, address, includeActors } _ ->
            hash (join [ "GetAddressEffect", currency, address, b includeActors ])

        GetEntityEffect { currency, entity } _ ->
            hash (join [ "GetEntityEffect", currency, i entity ])

        GetEntityEffectWithDetails { currency, entity, includeActors, includeBestTag } _ ->
            hash (join [ "GetEntityEffectWithDetails", currency, i entity, b includeActors, b includeBestTag ])

        GetActorEffect { actorId } _ ->
            hash (join [ "GetActorEffect", actorId ])

        GetBlockEffect { currency, height } _ ->
            hash (join [ "GetBlockEffect", currency, i height ])

        GetBlockByDateEffect { currency, datetime } _ ->
            hash (join [ "GetBlockByDateEffect", currency, p datetime ])

        GetEntityForAddressEffect { currency, address } _ ->
            hash (join [ "GetEntityForAddressEffect", currency, address ])

        GetEntityNeighborsEffect { currency, entity, isOutgoing, onlyIds, includeLabels, pagesize, nextpage } _ ->
            hash
                (join
                    [ "GetEntityNeighborsEffect"
                    , currency
                    , i entity
                    , b isOutgoing
                    , mb (List.map String.fromInt >> String.join ",") onlyIds
                    , b includeLabels
                    , i pagesize
                    , mbs nextpage
                    ]
                )

        GetAddressTxsEffect { currency, address, direction, minHeight, maxHeight, tokenCurrency, order, pagesize, nextpage } _ ->
            hash
                (join
                    [ "GetAddressTxsEffect"
                    , currency
                    , address
                    , mb dir direction
                    , mbi minHeight
                    , mbi maxHeight
                    , mbs tokenCurrency
                    , mb ord order
                    , i pagesize
                    , mbs nextpage
                    ]
                )

        GetEntityTxsEffect { currency, entity, pagesize, nextpage } _ ->
            hash (join [ "GetEntityTxsEffect", currency, i entity, i pagesize, mbs nextpage ])

        GetAddressTagSummaryEffect { currency, address, includeBestClusterTag } _ ->
            hash (join [ "GetAddressTagSummaryEffect", currency, address, b includeBestClusterTag ])

        GetActorTagsEffect { actorId, pagesize, nextpage } _ ->
            hash (join [ "GetActorTagsEffect", actorId, i pagesize, mbs nextpage ])

        GetBlockTxsEffect { currency, block, pagesize, nextpage } _ ->
            hash (join [ "GetBlockTxsEffect", currency, i block, i pagesize, mbs nextpage ])

        SearchEntityNeighborsEffect { currency, entity, isOutgoing, key, value, depth, breadth, maxAddresses } _ ->
            hash
                (join
                    [ "SearchEntityNeighborsEffect"
                    , currency
                    , i entity
                    , b isOutgoing
                    , ckey key
                    , String.join "," value
                    , i depth
                    , i breadth
                    , i maxAddresses
                    ]
                )

        GetTxEffect { currency, txHash, tokenTxId, includeIo } _ ->
            hash (join [ "GetTxEffect", currency, txHash, mbi tokenTxId, b includeIo ])

        GetTxUtxoAddressesEffect { currency, txHash, isOutgoing } _ ->
            hash (join [ "GetTxUtxoAddressesEffect", currency, txHash, b isOutgoing ])

        ListSpendingTxRefsEffect { currency, txHash, index } _ ->
            hash (join [ "ListSpendingTxRefsEffect", currency, txHash, mbi index ])

        ListSpentInTxRefsEffect { currency, txHash, index } _ ->
            hash (join [ "ListSpentInTxRefsEffect", currency, txHash, mbi index ])

        ListAddressTagsEffect { label, nextpage, pagesize } _ ->
            hash (join [ "ListAddressTagsEffect", label, mbs nextpage, mbi pagesize ])

        GetEntitylinkTxsEffect { currency, source, target, minHeight, maxHeight, order, nextpage, pagesize } _ ->
            hash
                (join
                    [ "GetEntitylinkTxsEffect"
                    , currency
                    , i source
                    , i target
                    , mbi minHeight
                    , mbi maxHeight
                    , mb cord order
                    , mbs nextpage
                    , i pagesize
                    ]
                )

        GetTokenTxsEffect { currency, txHash } _ ->
            hash (join [ "GetTokenTxsEffect", currency, txHash ])

        BulkGetAddressEffect { currency, addresses } _ ->
            hash (join [ "BulkGetAddressEffect", currency, String.join "," addresses ])

        BulkGetAddressTagsEffect { currency, addresses, pagesize, includeBestClusterTag } _ ->
            hash (join [ "BulkGetAddressTagsEffect", currency, String.join "," addresses, mbi pagesize, b includeBestClusterTag ])

        BulkGetEntityEffect { currency, entities } _ ->
            hash (join [ "BulkGetEntityEffect", currency, String.join "," (List.map String.fromInt entities) ])

        BulkGetAddressEntityEffect { currency, addresses } _ ->
            hash (join [ "BulkGetAddressEntityEffect", currency, String.join "," addresses ])

        BulkGetEntityNeighborsEffect { currency, isOutgoing, entities, onlyIds } _ ->
            hash
                (join
                    [ "BulkGetEntityNeighborsEffect"
                    , currency
                    , b isOutgoing
                    , String.join "," (List.map String.fromInt entities)
                    , b onlyIds
                    ]
                )

        BulkGetAddressNeighborsEffect { currency, isOutgoing, addresses, onlyIds } _ ->
            hash
                (join
                    [ "BulkGetAddressNeighborsEffect"
                    , currency
                    , b isOutgoing
                    , String.join "," addresses
                    , mb (String.join ",") onlyIds
                    ]
                )

        BulkGetTxEffect { currency, txs } _ ->
            hash (join [ "BulkGetTxEffect", currency, String.join "," txs ])

        BulkGetAddressTagSummaryEffect { currency, addresses, includeBestClusterTag } _ ->
            hash (join [ "BulkGetAddressTagSummaryEffect", currency, String.join "," addresses, b includeBestClusterTag ])

        ListRelatedAddressesEffect { currency, address, reltype, pagesize, nextpage } _ ->
            hash (join [ "ListRelatedAddressesEffect", currency, address, rel reltype, i pagesize, mbs nextpage ])

        GetConversionEffect { currency, txHash } _ ->
            hash (join [ "GetConversionEffect", currency, txHash ])

        ListTxFlowsEffect { currency, txHash, includeZeroValueSubTxs, token_currency, pagesize, nextpage } _ ->
            hash
                (join
                    [ "ListTxFlowsEffect"
                    , currency
                    , txHash
                    , b includeZeroValueSubTxs
                    , mbs token_currency
                    , mbi pagesize
                    , mbs nextpage
                    ]
                )


withAuthorization : String -> Api.Request a -> Api.Request a
withAuthorization apiKey request =
    if String.isEmpty apiKey then
        request

    else
        Api.withHeader "Authorization" apiKey request


send : String -> (Result ( Http.Error, Headers, eff ) ( Headers, msg ) -> msg) -> eff -> (a -> msg) -> Api.Request a -> Cmd msg
send apiKey wrapMsg effect toMsg =
    withAuthorization apiKey
        >> Api.sendAndAlsoReceiveHeaders wrapMsg effect toMsg


isOutgoingToDirection : Bool -> Api.Request.Clusters.Direction
isOutgoingToDirection isOutgoing =
    if isOutgoing then
        Api.Request.Clusters.DirectionOut

    else
        Api.Request.Clusters.DirectionIn


isOutgoingToAddressDirection : Bool -> Api.Request.Addresses.Direction
isOutgoingToAddressDirection isOutgoing =
    if isOutgoing then
        Api.Request.Addresses.DirectionOut

    else
        Api.Request.Addresses.DirectionIn


userInfoDecoder : Json.Decode.Decoder UserInfo
userInfoDecoder =
    Json.Decode.map
        (\expiration ->
            { expiration = expiration
            }
        )
        (Json.Decode.oneOf
            [ Json.Decode.field "expires" expiresDecoder
            , Json.Decode.succeed Nothing
            ]
        )


expiresDecoder : Json.Decode.Decoder (Maybe Time.Posix)
expiresDecoder =
    Json.Decode.oneOf
        [ Json.Decode.null Nothing
        , dateTimeDecoder
            |> Json.Decode.map Just
        , Json.Decode.succeed Nothing
        ]


listWithMaybes : Json.Decode.Decoder a -> Json.Decode.Decoder (List a)
listWithMaybes decoder =
    Json.Decode.list (Json.Decode.maybe decoder)
        |> Json.Decode.map (List.filterMap identity)
