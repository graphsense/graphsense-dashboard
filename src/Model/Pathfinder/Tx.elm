module Model.Pathfinder.Tx exposing
    ( AccountTx
    , ConversionLegType(..)
    , Io
    , Tx
    , TxType(..)
    , UnsupportedConversion
    , UtxoTx
    , getAccountTx
    , getAccountTxRaw
    , getAssetFromRawTx
    , getCoords
    , getInputAddressIds
    , getInputValueForAddressFromRawTx
    , getInputs
    , getNetwork
    , getOutputAddressIds
    , getOutputValueForAddressFromRawTx
    , getOutputs
    , getRawBaseTxHashForTx
    , getRawTimestamp
    , getRawTimestampForRelationTx
    , getRawTx
    , getTxId
    , getTxIdForAddressTx
    , getTxIdForRelationTx
    , getTxIdForTx
    , getTxIdForTxType
    , getUtxoTx
    , hasInput
    , hasOutput
    , ioToId
    , isRawInFlow
    , isRawOutFlow
    , isZeroValueTx
    , listAddressesForTx
    , listSeparatedAddressesForTx
    , toFinalCoords
    , utxoTxToAccountTxs
    )

import Animation exposing (Animation, Clock)
import Api.Data
import Dict exposing (Dict)
import Init.Pathfinder.Id as Id
import List.Extra
import Model.Direction exposing (Direction(..))
import Model.Graph.Coords exposing (Coords)
import Model.Locale as Locale
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id as Id exposing (Id)
import Set
import Tuple exposing (pair)
import Util.Data as Data
import View.Locale as Locale


type ConversionLegType
    = InputLegConversion
    | OutputLegConversion


type alias Tx =
    { id : Id
    , hovered : Bool
    , selected : Bool
    , type_ : TxType
    , x : Float
    , y : Animation
    , dx : Float
    , dy : Float
    , clock : Clock
    , opacity : Animation
    , isStartingPoint : Bool
    , conversionType : Maybe ConversionLegType
    , unsupportedConversions : List UnsupportedConversion
    , index : Int
    }


type alias UnsupportedConversion =
    { toAddress : String
    , toNetwork : String
    , conversionType : Api.Data.ExternalConversionConversionType
    }


type TxType
    = Account AccountTx
    | Utxo UtxoTx


type alias AccountTx =
    { from : Id
    , to : Id
    , fromAddress : Maybe Address -- the address present on the graph
    , toAddress : Maybe Address -- the address present on the graph
    , value : Api.Data.Values
    , raw : Api.Data.TxAccount
    }


type alias UtxoTx =
    { inputs : Dict Id Io
    , outputs : Dict Id Io
    , raw : Api.Data.TxUtxo
    }


type alias Io =
    { values : Api.Data.Values
    , address : Maybe Address -- the address present on the graph
    , aggregatesN : Int
    }


getRawTx : Tx -> Api.Data.Tx
getRawTx tx =
    case tx.type_ of
        Account { raw } ->
            Api.Data.TxTxAccount raw

        Utxo { raw } ->
            Api.Data.TxTxUtxo raw


getAssetFromRawTx : Api.Data.Tx -> String
getAssetFromRawTx tx =
    case tx of
        Api.Data.TxTxAccount { currency } ->
            currency

        Api.Data.TxTxUtxo { currency } ->
            currency


getNetwork : Tx -> String
getNetwork tx =
    getRawBaseNetworkForTxType tx.type_


isRawInFlow : Id -> Tx -> Bool
isRawInFlow id tx =
    case tx.type_ of
        Account { raw } ->
            raw.fromAddress == Id.id id

        Utxo { raw } ->
            raw.inputs
                |> findTxValueByAddress (Id.id id)
                |> (/=) Nothing


isRawOutFlow : Id -> Tx -> Bool
isRawOutFlow id tx =
    case tx.type_ of
        Account { raw } ->
            raw.toAddress == Id.id id

        Utxo { raw } ->
            raw.outputs
                |> findTxValueByAddress (Id.id id)
                |> (/=) Nothing


findTxValueByAddress : String -> Maybe (List Api.Data.TxValue) -> Maybe Api.Data.TxValue
findTxValueByAddress id =
    Maybe.andThen
        (List.Extra.find (.address >> List.member id))


hasOutput : Id -> Tx -> Bool
hasOutput id tx =
    case tx.type_ of
        Account { to } ->
            to == id

        Utxo { outputs } ->
            Dict.get id outputs /= Nothing


hasInput : Id -> Tx -> Bool
hasInput id tx =
    case tx.type_ of
        Account { from } ->
            from == id

        Utxo { inputs } ->
            Dict.get id inputs /= Nothing


listAddressesForTx : Tx -> List ( Direction, Address )
listAddressesForTx tx =
    case tx.type_ of
        Account { fromAddress, toAddress } ->
            [ fromAddress
                |> Maybe.map (pair Incoming)
            , toAddress
                |> Maybe.map (pair Outgoing)
            ]
                |> List.filterMap identity

        Utxo { inputs, outputs } ->
            (Dict.values inputs
                |> List.filterMap .address
                |> List.map (pair Incoming)
            )
                ++ (Dict.values outputs
                        |> List.filterMap .address
                        |> List.map (pair Outgoing)
                   )


listSeparatedAddressesForTx : Tx -> ( List Address, List Address )
listSeparatedAddressesForTx tx =
    case tx.type_ of
        Account { fromAddress, toAddress } ->
            ( fromAddress
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
            , toAddress
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
            )

        Utxo { inputs, outputs } ->
            ( Dict.values inputs
                |> List.filterMap .address
            , Dict.values outputs
                |> List.filterMap .address
            )


getInputAddressIds : Tx -> List Id
getInputAddressIds tx =
    case tx.type_ of
        Account { from } ->
            [ from ]

        Utxo { raw } ->
            raw.inputs
                |> Maybe.withDefault []
                |> List.concatMap .address
                |> List.map (Id.init raw.currency)


getOutputAddressIds : Tx -> List Id
getOutputAddressIds tx =
    case tx.type_ of
        Account { to } ->
            [ to ]

        Utxo { raw } ->
            raw.outputs
                |> Maybe.withDefault []
                |> List.concatMap .address
                |> List.map (Id.init raw.currency)


getCoords : Tx -> Maybe Coords
getCoords tx =
    Coords tx.x (Animation.animate tx.clock tx.y)
        |> Just


toFinalCoords : { t | x : Float, y : Animation } -> Coords
toFinalCoords { x, y } =
    Coords x (Animation.getTo y)


getUtxoTx : Tx -> Maybe UtxoTx
getUtxoTx { type_ } =
    case type_ of
        Utxo t ->
            Just t

        Account _ ->
            Nothing


getAccountTxRaw : Api.Data.Tx -> Maybe Api.Data.TxAccount
getAccountTxRaw tx =
    case tx of
        Api.Data.TxTxAccount t ->
            Just t

        _ ->
            Nothing


getAccountTx : Tx -> Maybe AccountTx
getAccountTx { type_ } =
    case type_ of
        Utxo _ ->
            Nothing

        Account t ->
            Just t


getRawTimestamp : Tx -> Int
getRawTimestamp tx =
    case tx.type_ of
        Account t ->
            t.raw.timestamp

        Utxo t ->
            t.raw.timestamp


getTxId : Api.Data.Tx -> Id
getTxId tx =
    case tx of
        Api.Data.TxTxAccount t ->
            Id.init t.network t.identifier

        Api.Data.TxTxUtxo t ->
            Id.init t.currency t.txHash


getTxIdForTx : Tx -> Id
getTxIdForTx tx =
    getTxIdForTxType tx.type_


getTxIdForTxType : TxType -> Id
getTxIdForTxType type_ =
    case type_ of
        Account t ->
            Id.init t.raw.network t.raw.identifier

        Utxo t ->
            Id.init t.raw.currency t.raw.txHash


getRawBaseTxHashForTx : Tx -> String
getRawBaseTxHashForTx tx =
    getRawBaseTxHashForTxType tx.type_


getRawBaseTxHashForTxType : TxType -> String
getRawBaseTxHashForTxType tx =
    case tx of
        Account t ->
            t.raw.txHash

        Utxo t ->
            t.raw.txHash


getRawBaseNetworkForTxType : TxType -> String
getRawBaseNetworkForTxType tx =
    case tx of
        Account t ->
            t.raw.network

        Utxo t ->
            t.raw.currency


getTxIdForAddressTx : Api.Data.AddressTx -> Id
getTxIdForAddressTx tx =
    case tx of
        Api.Data.AddressTxTxAccount t ->
            Id.init t.network t.identifier

        Api.Data.AddressTxAddressTxUtxo t ->
            Id.init t.currency t.txHash


getTxIdForRelationTx : Api.Data.Link -> Id
getTxIdForRelationTx tx =
    case tx of
        Api.Data.LinkTxAccount t ->
            Id.init t.network t.identifier

        Api.Data.LinkLinkUtxo t ->
            Id.init t.currency t.txHash


getRawTimestampForRelationTx : Api.Data.Link -> Int
getRawTimestampForRelationTx tx =
    case tx of
        Api.Data.LinkTxAccount t ->
            t.timestamp

        Api.Data.LinkLinkUtxo t ->
            t.timestamp


ioToId : String -> Api.Data.TxValue -> Maybe Id
ioToId network =
    .address
        >> List.head
        >> Maybe.map (Id.init network)


getOutputs : Tx -> List Id
getOutputs tx =
    case tx.type_ of
        Utxo { outputs } ->
            Dict.keys outputs

        Account { to } ->
            [ to ]


getInputs : Tx -> List Id
getInputs tx =
    case tx.type_ of
        Utxo { inputs } ->
            Dict.keys inputs

        Account { from } ->
            [ from ]


getInputValueForAddressFromRawTx : String -> Api.Data.Tx -> Api.Data.Values
getInputValueForAddressFromRawTx address tx =
    case tx of
        Api.Data.TxTxAccount { fromAddress, value } ->
            if String.toLower fromAddress == String.toLower address then
                value

            else
                Data.valuesZero

        Api.Data.TxTxUtxo { inputs } ->
            inputs
                |> Maybe.withDefault []
                |> List.filter (.address >> List.map String.toLower >> Set.fromList >> Set.member (String.toLower address))
                |> List.map .value
                |> Data.sumValues


getOutputValueForAddressFromRawTx : String -> Api.Data.Tx -> Api.Data.Values
getOutputValueForAddressFromRawTx address tx =
    case tx of
        Api.Data.TxTxAccount { toAddress, value } ->
            if String.toLower toAddress == String.toLower address then
                value

            else
                Data.valuesZero

        Api.Data.TxTxUtxo { outputs } ->
            outputs
                |> Maybe.withDefault []
                |> List.filter (.address >> List.map String.toLower >> Set.fromList >> Set.member (String.toLower address))
                |> List.map .value
                |> Data.sumValues


isZeroValueTx : Api.Data.Tx -> Bool
isZeroValueTx tx =
    case tx of
        Api.Data.TxTxAccount { value } ->
            value.value == 0

        Api.Data.TxTxUtxo _ ->
            False


{-| Convert a UTXO transaction into per-flow account-style rows.

By default every input is paired with every output (plus a fee row per input),
i.e. the full N×M cartesian product. For transactions with many inputs and
outputs this explodes (e.g. 350×350 ≈ 120k rows for a single tx).

`options` controls the reduction:

  - `onlyVisibleIos`: when `True`, only flows that have an on-graph (visible)
    address on _both_ sides are emitted, regardless of size, and the per-input
    fee rows are dropped. This is a user-requested filter, not a forced cap.
  - `rowCap`: `Just maxRows` bounds the number of rows. When the output would
    exceed it, the result falls back to the on-graph-only flows and, if that is
    still too many, is truncated to `maxRows`.

The result reports two flags: `capped` is `True` when the **forced** row cap
truncated the output (this is the surprising case worth surfacing to the user),
and `reduced` is `True` whenever fewer rows than the full product were emitted
(forced cap and/or the visible-only filter) — used to annotate the CSV.

-}
utxoTxToAccountTxs : { onlyVisibleIos : Bool, rowCap : Maybe Int } -> Maybe Locale.Model -> UtxoTx -> { rows : List Api.Data.TxAccount, capped : Bool, reduced : Bool }
utxoTxToAccountTxs options locale utxoTx =
    let
        raw =
            utxoTx.raw

        inputs =
            utxoTx.inputs

        outputs =
            utxoTx.outputs

        sumInputs =
            Dict.values inputs
                |> List.map .values
                |> Data.sumValues

        sumOutputs =
            Dict.values outputs
                |> List.map .values
                |> Data.sumValues

        fee =
            Data.subValues sumInputs sumOutputs

        hasFeeRow =
            case locale of
                Just _ ->
                    fee.value /= 0

                Nothing ->
                    False

        -- Rows for a single input paired with the given outputs. `includeFee`
        -- adds the per-input fee row (only meaningful when the input side is
        -- itself part of the export, i.e. on-graph or unfiltered).
        rowsForInput includeFee outs ( inputId, inputIo ) =
            let
                inputPortion =
                    if sumInputs.value > 0 then
                        toFloat inputIo.values.value / toFloat sumInputs.value

                    else
                        0

                outputRows =
                    outs
                        |> List.map
                            (\( outputId, outputIo ) ->
                                { contractCreation = Nothing
                                , currency = raw.currency
                                , fromAddress = Id.id inputId
                                , height = raw.height
                                , identifier = ""
                                , isExternal = Nothing
                                , network = raw.currency
                                , timestamp = raw.timestamp
                                , toAddress = Id.id outputId
                                , tokenTxId = Nothing
                                , txHash = raw.txHash
                                , txType = "utxo"
                                , value = Data.mulValues inputPortion outputIo.values
                                , fee = Nothing
                                }
                            )

                feeRows =
                    case locale of
                        Just loc ->
                            if includeFee && hasFeeRow then
                                [ { contractCreation = Nothing
                                  , currency = raw.currency
                                  , fromAddress = Id.id inputId
                                  , height = raw.height
                                  , identifier = ""
                                  , isExternal = Nothing
                                  , network = raw.currency
                                  , timestamp = raw.timestamp
                                  , toAddress = Locale.string loc "fee"
                                  , tokenTxId = Nothing
                                  , txHash = raw.txHash
                                  , txType = "utxo"
                                  , value = Data.mulValues inputPortion (Data.negateValues fee)
                                  , fee = Nothing
                                  }
                                ]

                            else
                                []

                        Nothing ->
                            []
            in
            outputRows ++ feeRows

        allInputs =
            Dict.toList inputs

        allOutputs =
            Dict.toList outputs

        -- Number of rows the full cartesian product would produce, computed
        -- arithmetically so we never materialize the explosion just to size it.
        fullRowCount =
            (List.length allInputs * List.length allOutputs)
                + (if hasFeeRow then
                    List.length allInputs

                   else
                    0
                  )

        buildFull () =
            allInputs |> List.concatMap (rowsForInput True allOutputs)

        onGraph ( _, io ) =
            io.address /= Nothing

        -- Flows with an on-graph (visible) address on *both* sides: on-graph
        -- inputs paired only with on-graph outputs. The per-input fee row is
        -- dropped here, since the fee is not a flow between two graph
        -- addresses.
        buildOnGraphOnly () =
            let
                onGraphInputs =
                    List.filter onGraph allInputs

                onGraphOutputs =
                    List.filter onGraph allOutputs
            in
            onGraphInputs |> List.concatMap (rowsForInput False onGraphOutputs)

        reducedFull rows =
            List.length rows < fullRowCount
    in
    case options.rowCap of
        Just maxRows ->
            if options.onlyVisibleIos then
                let
                    visible =
                        buildOnGraphOnly ()
                in
                if List.length visible > maxRows then
                    { rows = List.take maxRows visible, capped = True, reduced = True }

                else
                    { rows = visible, capped = False, reduced = reducedFull visible }

            else if fullRowCount > maxRows then
                { rows = buildOnGraphOnly () |> List.take maxRows, capped = True, reduced = True }

            else
                { rows = buildFull (), capped = False, reduced = False }

        Nothing ->
            if options.onlyVisibleIos then
                let
                    visible =
                        buildOnGraphOnly ()
                in
                { rows = visible, capped = False, reduced = reducedFull visible }

            else
                { rows = buildFull (), capped = False, reduced = False }
