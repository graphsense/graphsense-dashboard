module Model.Pathfinder.GraphFilter exposing
    ( Model, TimeMode(..), Period, Config, Result, Bucket, Inputs
    , init, isActive, activeCount, emptyResult
    , apply, inputs, txFiatValue, assetsOnGraph, countBelowLimit
    , dayBounds, fromDateString, toDateString, activity
    )

{-| On-graph filters of the Pathfinder: hide transfers by value (fiat), time
and asset, and the addresses left without a visible transfer.

This module is pure: `apply` turns the filter settings and the network into
the sets the view needs. It is view state, so it is neither part of the undo
history nor saved in `.gs` files.

Rules:

  - A transaction passes when its asset, value and time all pass. The asset
    of a swap leg also passes when the other leg's asset passes, so a swap
    never breaks the trail ("swaps stay linked").
  - A transaction without a fiat value in the chosen currency is never hidden
    by the value filter: what we cannot price, we do not drop.
  - The value of a UTXO transaction is the largest input or output that is
    shown on the graph (an io whose address is on the graph). Without any,
    it is the total output. Provisional, to be checked in testing.
  - An address is hidden when it has transfers on the graph but none of them
    is visible. Protected addresses (starting points, plus what the caller
    passes: annotated, selected) are kept instead (`greyedAddresses`: drawn
    normally, dimmed in grey mode). Addresses without any
    transfer on the graph are never touched.
  - In the aggregated tracing mode, an edge passes on its total fiat value and
    its assets; the time filter does not apply (edge totals have no time).

@docs Model, TimeMode, Period, Config, Result, Bucket, Inputs
@docs init, isActive, activeCount, emptyResult
@docs apply, inputs, txFiatValue, assetsOnGraph, countBelowLimit
@docs dayBounds, fromDateString, toDateString, activity

-}

import Api.Data
import Config.Pathfinder exposing (TracingMode(..))
import Date
import Dict exposing (Dict)
import Model.Locale as Locale
import Model.Pathfinder.AggEdge exposing (AggEdge)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Network exposing (Network)
import Model.Pathfinder.Tx as Tx exposing (Tx, TxType(..))
import RemoteData
import Set exposing (Set)
import Time exposing (Posix)
import Time.Extra


{-| Inclusive on both ends.
-}
type alias Period =
    { from : Posix
    , until : Posix
    }


type alias Model =
    { hideTiny : Bool

    -- in the user's fiat currency; kept while `hideTiny` is off
    , limit : Float
    , timeMode : TimeMode
    , period : Maybe Period

    -- excluded rather than included, so assets added later show up
    , excludedAssets : Set String
    , showHiddenGrey : Bool

    -- the limit editor in the popover is open; not a filter
    , editingLimit : Bool

    -- the bucket a drag on the activity strip started on; not a filter
    , stripDrag : Maybe Period
    }


{-| How the period is picked; the period itself is what filters.
-}
type TimeMode
    = AnyTime
    | OneDay
    | DateRange


type alias Config =
    { fiatCode : String
    , tracingMode : TracingMode
    , protected : Set Id
    }


type alias Result =
    { hiddenTxs : Set Id
    , hiddenAggEdges : Set ( Id, Id )
    , hiddenConversions : Set ( Id, Id )
    , hiddenAddresses : Set Id
    , greyedAddresses : Set Id
    }


init : Model
init =
    { hideTiny = False
    , limit = 10
    , timeMode = AnyTime
    , period = Nothing
    , excludedAssets = Set.empty
    , showHiddenGrey = False
    , editingLimit = False
    , stripDrag = Nothing
    }


emptyResult : Result
emptyResult =
    { hiddenTxs = Set.empty
    , hiddenAggEdges = Set.empty
    , hiddenConversions = Set.empty
    , hiddenAddresses = Set.empty
    , greyedAddresses = Set.empty
    }


{-| Number of active filters, for the toolbar badge and the chips.
-}
activeCount : Model -> Int
activeCount m =
    [ m.hideTiny
    , m.period /= Nothing
    , not (Set.isEmpty m.excludedAssets)
    ]
        |> List.filter identity
        |> List.length


isActive : Model -> Bool
isActive =
    activeCount >> (<) 0


apply : Config -> Model -> Network -> Result
apply config model network =
    if not (isActive model) then
        emptyResult

    else
        case config.tracingMode of
            TransactionTracingMode ->
                applyTx config model network

            AggregateTracingMode ->
                applyAgg config model network


applyTx : Config -> Model -> Network -> Result
applyTx config model network =
    let
        partners =
            swapPartnerAssets network

        hiddenTxs =
            network.txs
                |> Dict.filter (\_ tx -> not (txPasses config model partners tx))
                |> Dict.keys
                |> Set.fromList

        ( touched, active ) =
            network.txs
                |> Dict.foldl
                    (\id tx ( t, a ) ->
                        let
                            ids =
                                Tx.listAddressesForTx tx
                                    |> List.map (Tuple.second >> .id)
                                    |> Set.fromList
                        in
                        ( Set.union ids t
                        , if Set.member id hiddenTxs then
                            a

                          else
                            Set.union ids a
                        )
                    )
                    ( Set.empty, Set.empty )

        ( hiddenAddresses, greyedAddresses ) =
            splitProtected config network (Set.diff touched active)
    in
    { emptyResult
        | hiddenTxs = hiddenTxs
        , hiddenConversions =
            network.conversions
                |> Dict.filter
                    (\( input, output ) _ ->
                        Set.member input hiddenTxs || Set.member output hiddenTxs
                    )
                |> Dict.keys
                |> Set.fromList
        , hiddenAddresses = hiddenAddresses
        , greyedAddresses = greyedAddresses
    }


applyAgg : Config -> Model -> Network -> Result
applyAgg config model network =
    let
        hiddenAggEdges =
            network.aggEdges
                |> Dict.filter (\_ edge -> not (aggEdgePasses config model edge))
                |> Dict.keys
                |> Set.fromList

        ( touched, active ) =
            network.aggEdges
                |> Dict.foldl
                    (\key edge ( t, a ) ->
                        let
                            ids =
                                Set.fromList [ edge.a, edge.b ]
                        in
                        ( Set.union ids t
                        , if Set.member key hiddenAggEdges then
                            a

                          else
                            Set.union ids a
                        )
                    )
                    ( Set.empty, Set.empty )

        ( hiddenAddresses, greyedAddresses ) =
            splitProtected config network (Set.diff touched active)
    in
    { emptyResult
        | hiddenAggEdges = hiddenAggEdges
        , hiddenAddresses = hiddenAddresses
        , greyedAddresses = greyedAddresses
    }


splitProtected : Config -> Network -> Set Id -> ( Set Id, Set Id )
splitProtected config network =
    Set.partition
        (\id ->
            not
                (Set.member id config.protected
                    || (Dict.get id network.addresses
                            |> Maybe.map .isStartingPoint
                            |> Maybe.withDefault False
                       )
                )
        )


{-| What `apply` depends on, without what changes on every pan, drag or
hover (positions, hover and selection flags). Equal inputs give an equal
result, so a caller can skip `apply`, which costs milliseconds on a large
graph, while the user only moves things around.
-}
type alias Inputs =
    { config : Config
    , model : Model
    , txs : List Id

    -- with whether it is a starting point
    , addresses : List ( Id, Bool )

    -- with whether both directions have loaded
    , aggEdges : List ( ( Id, Id ), Bool )
    , conversions : List ( Id, Id )
    }


inputs : Config -> Model -> Network -> Inputs
inputs config model network =
    { config = config
    , model = model
    , txs = Dict.keys network.txs
    , addresses = Dict.foldr (\id a acc -> ( id, a.isStartingPoint ) :: acc) [] network.addresses
    , aggEdges =
        Dict.foldr
            (\key e acc -> ( key, RemoteData.isSuccess e.a2b && RemoteData.isSuccess e.b2a ) :: acc)
            []
            network.aggEdges
    , conversions = Dict.keys network.conversions
    }



-- TRANSACTIONS


txPasses : Config -> Model -> Dict Id String -> Tx -> Bool
txPasses config model partners tx =
    assetPasses model partners tx
        && valuePasses model (txFiatValue config.fiatCode tx)
        && timePasses model (Tx.getRawTimestamp tx)


assetPasses : Model -> Dict Id String -> Tx -> Bool
assetPasses model partners tx =
    let
        included asset =
            not (Set.member asset model.excludedAssets)
    in
    included (txAsset tx)
        || (Dict.get tx.id partners
                |> Maybe.map included
                |> Maybe.withDefault False
           )


valuePasses : Model -> Maybe Float -> Bool
valuePasses model value =
    case ( model.hideTiny, value ) of
        ( True, Just v ) ->
            v >= model.limit

        _ ->
            True


timePasses : Model -> Int -> Bool
timePasses model seconds =
    case model.period of
        Nothing ->
            True

        Just { from, until } ->
            let
                ms =
                    seconds * 1000
            in
            ms >= Time.posixToMillis from && ms <= Time.posixToMillis until


{-| The fiat value the value filter compares, see the module docs.
-}
txFiatValue : String -> Tx -> Maybe Float
txFiatValue fiatCode tx =
    case tx.type_ of
        Account t ->
            Locale.getFiatValue fiatCode t.value

        Utxo t ->
            let
                shown =
                    Dict.values t.inputs
                        ++ Dict.values t.outputs
                        |> List.filter (.address >> (/=) Nothing)
                        |> List.filterMap (.values >> Locale.getFiatValue fiatCode)
                        |> List.maximum
            in
            case shown of
                Just v ->
                    Just v

                Nothing ->
                    Locale.getFiatValue fiatCode t.raw.totalOutput


txAsset : Tx -> String
txAsset =
    Tx.getRawTx >> Tx.getAssetFromRawTx >> String.toLower


{-| For each swap leg on the graph, the asset of the other leg.
-}
swapPartnerAssets : Network -> Dict Id String
swapPartnerAssets network =
    network.conversions
        |> Dict.foldl
            (\( input, output ) conversion acc ->
                acc
                    |> Dict.insert input (legAsset network output conversion.raw.toAssetSymbol)
                    |> Dict.insert output (legAsset network input conversion.raw.fromAssetSymbol)
            )
            Dict.empty


legAsset : Network -> Id -> Maybe String -> String
legAsset network id symbol =
    case Dict.get id network.txs of
        Just tx ->
            txAsset tx

        Nothing ->
            symbol
                |> Maybe.withDefault (Id.network id)
                |> String.toLower



-- AGGREGATED EDGES


aggEdgePasses : Config -> Model -> AggEdge -> Bool
aggEdgePasses config model edge =
    let
        values =
            aggEdgeValues edge

        included =
            values
                |> List.filter (\( asset, _ ) -> not (Set.member asset model.excludedAssets))

        assetOk =
            List.isEmpty values
                || List.any (\( _, v ) -> v.value /= 0) included

        fiat =
            included
                |> List.map (Tuple.second >> Locale.getFiatValue config.fiatCode)
                |> List.foldl (Maybe.map2 (+)) (Just 0)
    in
    assetOk
        && (List.isEmpty values || valuePasses model fiat)


{-| Per asset, both directions together.
-}
aggEdgeValues : AggEdge -> List ( String, Api.Data.Values )
aggEdgeValues edge =
    let
        native =
            Id.network edge.a |> String.toLower

        ofDirection =
            RemoteData.toMaybe
                >> Maybe.andThen identity
                >> Maybe.map
                    (\n ->
                        ( native, n.value )
                            :: (n.tokenValues
                                    |> Maybe.withDefault Dict.empty
                                    |> Dict.toList
                                    |> List.map (Tuple.mapFirst String.toLower)
                               )
                    )
                >> Maybe.withDefault []
    in
    (ofDirection edge.a2b ++ ofDirection edge.b2a)
        |> List.foldl
            (\( asset, v ) acc ->
                Dict.update asset
                    (Maybe.map (addValues v) >> Maybe.withDefault v >> Just)
                    acc
            )
            Dict.empty
        |> Dict.toList


addValues : Api.Data.Values -> Api.Data.Values -> Api.Data.Values
addValues a b =
    { value = a.value + b.value
    , fiatValues =
        a.fiatValues
            |> List.map
                (\r ->
                    { r
                        | value =
                            r.value
                                + (b.fiatValues
                                    |> List.filter (.code >> (==) r.code)
                                    |> List.map .value
                                    |> List.sum
                                  )
                    }
                )
    }



-- ASSETS


{-| The assets on the graph, for the asset choice in the popover.
-}
assetsOnGraph : TracingMode -> Network -> List String
assetsOnGraph mode network =
    (case mode of
        TransactionTracingMode ->
            Dict.values network.txs |> List.map txAsset

        AggregateTracingMode ->
            Dict.values network.aggEdges |> List.concatMap (aggEdgeValues >> List.map Tuple.first)
    )
        |> Set.fromList
        |> Set.toList


{-| How many transfers (or aggregated edges) on the graph are worth less than
`limit`, whether or not "hide tiny transfers" is on: the popover says how much
the switch hides before it is flipped. Unpriced ones never count.
-}
countBelowLimit : String -> TracingMode -> Float -> Network -> Int
countBelowLimit fiatCode mode limit network =
    let
        below =
            Maybe.map (\v -> v < limit) >> Maybe.withDefault False
    in
    case mode of
        TransactionTracingMode ->
            network.txs
                |> Dict.values
                |> List.filter (txFiatValue fiatCode >> below)
                |> List.length

        AggregateTracingMode ->
            network.aggEdges
                |> Dict.values
                |> List.filter
                    (\edge ->
                        case aggEdgeValues edge of
                            [] ->
                                False

                            values ->
                                values
                                    |> List.map (Tuple.second >> Locale.getFiatValue fiatCode)
                                    |> List.foldl (Maybe.map2 (+)) (Just 0)
                                    |> below
                    )
                |> List.length



-- TIME


{-| The whole day `t` falls on, in `zone`.
-}
dayBounds : Time.Zone -> Posix -> Period
dayBounds zone t =
    let
        from =
            Time.Extra.floor Time.Extra.Day zone t
    in
    { from = from
    , until = Time.Extra.add Time.Extra.Day 1 zone from |> Time.posixToMillis |> (+) -1 |> Time.millisToPosix
    }


{-| The start of the day a date field holds ("2026-03-02"), in `zone`.
-}
fromDateString : Time.Zone -> String -> Maybe Posix
fromDateString zone str =
    Date.fromIsoString str
        |> Result.toMaybe
        |> Maybe.map
            (\d ->
                Time.Extra.partsToPosix zone
                    { year = Date.year d
                    , month = Date.month d
                    , day = Date.day d
                    , hour = 0
                    , minute = 0
                    , second = 0
                    , millisecond = 0
                    }
            )


{-| The value for a date field.
-}
toDateString : Time.Zone -> Posix -> String
toDateString zone =
    Date.fromPosix zone >> Date.toIsoString


{-| One bar of the activity strip: the fiat value moved in `period`.
-}
type alias Bucket =
    { period : Period
    , value : Float
    }


{-| Money moved per day on the graph, for the activity strip: from the day of
the first transfer to the day of the last, one bucket per day, or per several
days when that would make more than `maxBuckets` bars. Only transfers that
pass the asset and value filters count, so the strip shows what the time
filter picks from. Empty in the aggregated mode, which has no times.
-}
activity : String -> Time.Zone -> Int -> TracingMode -> Model -> Network -> List Bucket
activity fiatCode zone maxBuckets mode model network =
    let
        partners =
            swapPartnerAssets network

        counted =
            network.txs
                |> Dict.values
                |> List.filter
                    (\tx ->
                        assetPasses model partners tx
                            && valuePasses model (txFiatValue fiatCode tx)
                    )
                |> List.map
                    (\tx ->
                        ( Tx.getRawTimestamp tx * 1000 |> Time.millisToPosix
                        , txFiatValue fiatCode tx |> Maybe.withDefault 0
                        )
                    )

        times =
            List.map (Tuple.first >> Time.posixToMillis) counted
    in
    case ( mode, List.minimum times, List.maximum times ) of
        ( TransactionTracingMode, Just first, Just last ) ->
            let
                start =
                    Time.Extra.floor Time.Extra.Day zone (Time.millisToPosix first)

                days =
                    Time.Extra.diff Time.Extra.Day zone start (Time.millisToPosix last) + 1

                daysPerBucket =
                    max 1 (ceiling (toFloat days / toFloat (max 1 maxBuckets)))

                bucketCount =
                    ceiling (toFloat days / toFloat daysPerBucket)

                bucketOf t =
                    Time.Extra.diff Time.Extra.Day zone start t // daysPerBucket

                sums =
                    counted
                        |> List.foldl
                            (\( t, v ) -> Dict.update (bucketOf t) (Maybe.withDefault 0 >> (+) v >> Just))
                            Dict.empty
            in
            List.range 0 (bucketCount - 1)
                |> List.map
                    (\i ->
                        let
                            from =
                                Time.Extra.add Time.Extra.Day (i * daysPerBucket) zone start
                        in
                        { period =
                            { from = from
                            , until =
                                Time.Extra.add Time.Extra.Day daysPerBucket zone from
                                    |> Time.posixToMillis
                                    |> (+) -1
                                    |> Time.millisToPosix
                            }
                        , value = Dict.get i sums |> Maybe.withDefault 0
                        }
                    )

        _ ->
            []
